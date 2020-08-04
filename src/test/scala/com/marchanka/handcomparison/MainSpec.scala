package com.marchanka.handcomparison

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MainSpec extends AnyWordSpec with Matchers {

  "Class" should {
    "determine royal flush" in {
      val cards = Vector(Card(10, 1), Card(11, 1), Card(12, 1), Card(13, 1), Card(14, 1))
      val combination = CardCombination(cards)
      combination.combinationId shouldBe 10
    }

    "determine straight flush" in {
      val cards = Vector(Card(9, 1), Card(10, 1), Card(11, 1), Card(12, 1), Card(13, 1))
      val combination = CardCombination(cards)
      combination.combinationId shouldBe 9
    }

    "determine four of kind" in {
      val cards = Vector(Card(4, 1), Card(4, 2), Card(4, 3), Card(4, 4), Card(14, 1))
      val combination = CardCombination(cards)
      combination.combinationId shouldBe 8
    }

    "determine full house" in {
      val cards = Vector(Card(4, 1), Card(4, 2), Card(4, 3), Card(5, 4), Card(5, 1))
      val combination = CardCombination(cards)
      combination.combinationId shouldBe 7
    }

    "determine flush" in {
      val cards = Vector(Card(4, 1), Card(7, 1), Card(8, 1), Card(10, 1), Card(2, 1))
      val combination = CardCombination(cards)
      combination.combinationId shouldBe 6
    }

    "determine straight" in {
      val cards = Vector(Card(7, 1), Card(8, 1), Card(9, 1), Card(10, 3), Card(11, 1))
      val combination = CardCombination(cards)
      combination.combinationId shouldBe 5
    }

    "determine three of kind" in {
      val cards = Vector(Card(4, 1), Card(4, 2), Card(4, 3), Card(10, 1), Card(2, 1))
      val combination = CardCombination(cards)
      combination.combinationId shouldBe 4
    }

    "determine two pair" in {
      val cards = Vector(Card(4, 1), Card(4, 2), Card(3, 3), Card(3, 1), Card(2, 1))
      val combination = CardCombination(cards)
      combination.combinationId shouldBe 3
    }

    "determine pair" in {
      val cards = Vector(Card(4, 1), Card(4, 2), Card(7, 3), Card(10, 1), Card(2, 1))
      val combination = CardCombination(cards)
      combination.combinationId shouldBe 2
    }

    "determine hire card" in {
      val cards = Vector(Card(4, 1), Card(3, 2), Card(7, 3), Card(10, 1), Card(2, 1))
      val combination = CardCombination(cards)
      combination.combinationId shouldBe 1
    }

    "compare high cards" in {
      val cards1 = Vector(Card(4, 1), Card(3, 2), Card(13, 3), Card(10, 1), Card(2, 1))
      val combination1 = CardCombination(cards1)

      val cards2 = Vector(Card(4, 1), Card(3, 2), Card(7, 3), Card(10, 1), Card(2, 1))
      val combination2 = CardCombination(cards2)

      assert(CardCombination.calculateRank(combination1) > CardCombination.calculateRank(combination2))
    }

    "compare straight with royal flush" in {
      val cards1 = Vector(Card(10, 1), Card(11, 1), Card(12, 1), Card(13, 1), Card(14, 1))
      val combination1 = CardCombination(cards1)

      val cards2 = Vector(Card(10, 1), Card(11, 1), Card(12, 1), Card(13, 1), Card(14, 1))
      val combination2 = CardCombination(cards2)

      assert(CardCombination.calculateRank(combination1) > CardCombination.calculateRank(combination2))
    }

  }

}
