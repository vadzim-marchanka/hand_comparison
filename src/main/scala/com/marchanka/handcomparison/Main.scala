package com.marchanka.handcomparison

import Card.emptyCardSet
import CardCombination.CardGroupByRank._
import ComposeFcnOps._

import scala.collection.SortedSet
import scala.io.StdIn

object Main extends App {

  val cards = StdIn.readLine().split(" ").take(5).map(Card.apply).toSeq

  println(CardCombination(cards))

}

case class Card(rank: Int, suit: Int)

object Card {

  def orderByRankSuit: Ordering[Card] = { (x, y) =>
    x.rank - y.rank match {
      case difference if difference == 0 => x.suit - y.suit
      case difference => difference
    }
  }

  implicit val defaultOrdering = orderByRankSuit.reverse

  val emptyCardSet = SortedSet[Card]()

  def apply(str: String): Card = {
    val rank = str.take(str.length - 1) match {
      case "j"    => 10
      case "q"    => 11
      case "k"    => 12
      case "a"    => 13
      case number => number.toInt
    }
    val suite = str.last match {
      case 'c' => 1
      case 'd' => 2
      case 'h' => 3
      case 's' => 4
    }
    new Card(rank, suite)
  }

}

case class CardCombination(combinationId: Int, mainCards: SortedSet[Card], secondaryCards: SortedSet[Card], remainingCards: SortedSet[Card])

object CardCombination {

  def apply(cards: Seq[Card]) = {
    val sortedCards = Card.emptyCardSet ++ cards

    val combine = combineRoyalFlush
      .orElse(combineStraightFlush)
      .orElse[SortedSet[Card], CardCombination]((groupCards _).andThenPartial(combineFourOfKind))
      .orElse[SortedSet[Card], CardCombination]((groupCards _).andThenPartial(combineFullHouse))
      .orElse(combineFlush)
      .orElse[SortedSet[Card], CardCombination](combineStraight)
      .orElse[SortedSet[Card], CardCombination]((groupCards _).andThenPartial(combineThreeOfKind))
      .orElse[SortedSet[Card], CardCombination]((groupCards _).andThenPartial(combineTwoPairs))
      .orElse[SortedSet[Card], CardCombination]((groupCards _).andThenPartial(combinePair))
      .orElse[SortedSet[Card], CardCombination]{
        case cards => new CardCombination(1, emptyCardSet, emptyCardSet, cards)
      }

    combine(sortedCards)
  }

  def isRoyalFlush(cards: SortedSet[Card]) = {
    isStraightFlush(cards) && cards.head.rank == 14
  }

  def combineRoyalFlush : PartialFunction[SortedSet[Card], CardCombination] = {
    case cards if isRoyalFlush(cards) => CardCombination(10, cards, emptyCardSet, emptyCardSet)
  }

  def isStraightFlush(cards: SortedSet[Card]) = {
    isSameSuit(cards) && isStraight(cards)
  }

  def combineStraightFlush: PartialFunction[SortedSet[Card], CardCombination] = {
    case cards if isStraightFlush(cards) => CardCombination(9, cards, emptyCardSet, emptyCardSet)
  }

  def isFourOfKind(cards: SortedSet[Card]): Boolean = {
    cards.groupBy(_.rank).exists {
      case (_, cards) => cards.size == 4
    }
  }

  case class CardGroupByRank(rank: Int, cards: SortedSet[Card], occurrence: Int)

  object CardGroupByRank {
    val orderByOccurrenceAndRank: Ordering[CardGroupByRank] = { (a, b) =>
      a.occurrence - b.occurrence match {
        case difference if difference == 0 => a.rank - b.rank
        case difference => difference
      }
    }

    implicit val defaultOrdering = orderByOccurrenceAndRank.reverse

    val emptyCardGroupByRank = SortedSet[CardGroupByRank]()

    def groupCards(cards: SortedSet[Card]): SortedSet[CardGroupByRank] = {
      emptyCardGroupByRank ++ cards.groupBy(_.rank).map {
        case (rank, cards) => CardGroupByRank(rank, cards, cards.size)
      }
    }
    
  }

  def combineFourOfKind: PartialFunction[SortedSet[CardGroupByRank], CardCombination] = {
      case g if g.head.occurrence == 4 =>
        CardCombination(8, g.head.cards, emptyCardSet, g.last.cards)
  }

  def combineFullHouse: PartialFunction[SortedSet[CardGroupByRank], CardCombination] = {
      case g if g.head.occurrence == 3 && g.last.occurrence == 2 =>
        CardCombination(7, g.head.cards, g.last.cards, emptyCardSet)
  }

  def combineFlush: PartialFunction[SortedSet[Card], CardCombination] = {
    case cards if isSameSuit(cards) => CardCombination(6, cards, emptyCardSet, emptyCardSet)
  }

  def isStraight(card: SortedSet[Card]): Boolean = {
    val sortedRank = card.map(_.rank)
    sortedRank.size == 5 && (sortedRank.last - sortedRank.head) == 4
  }

  def combineStraight: PartialFunction[SortedSet[Card], CardCombination] = {
    case cards if isStraight(cards) => CardCombination(5, cards, emptyCardSet, emptyCardSet)
  }

  def combineThreeOfKind: PartialFunction[SortedSet[CardGroupByRank], CardCombination] = {
      case g if g.head.occurrence == 3 && g.last.occurrence == 1 =>
        CardCombination(4, g.head.cards, emptyCardSet, g.tail.flatMap(_.cards))
  }

  def combineTwoPairs: PartialFunction[SortedSet[CardGroupByRank], CardCombination] = {
      case g if g.head.occurrence == 2 && g.tail.head.occurrence == 2 =>
        CardCombination(3, g.head.cards, g.tail.head.cards, g.last.cards)
  }

  def combinePair: PartialFunction[SortedSet[CardGroupByRank], CardCombination] = {
      case g if g.head.occurrence == 2 && g.tail.head.occurrence == 1 =>
        CardCombination(2, g.head.cards, emptyCardSet, g.tail.flatMap(_.cards))
  }

  def isSameSuit(cards: SortedSet[Card]): Boolean = {
    cards.map(_.suit).size == 1
  }

  def calculateRank(combination: CardCombination) = {
    ((combination.combinationId *
      RANK_MULTIPLIER + cardsRank(combination.mainCards)) *
      RANK_MULTIPLIER + cardsRank(combination.secondaryCards)) *
      RANK_MULTIPLIER + cardsRank(combination.remainingCards)
  }

  def cardsRank(card: SortedSet[Card]): Int = {
    card.map(_.rank).fold(0){
      (sum, current) => sum + current * 20
    }
  }

  val RANK_MULTIPLIER = Math.pow(20, 5)

}

object ComposeFcnOps {
  implicit class TotalCompose[A, B](f: Function[A, B]) {
    def andThenPartial[C](that: PartialFunction[B, C]): PartialFunction[A, C] =
      Function.unlift(x => Option(f(x)).flatMap(that.lift))
  }
}
