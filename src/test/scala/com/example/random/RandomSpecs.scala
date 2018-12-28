package com.example.random

import org.scalatest.WordSpec

class RandomSpecs extends WordSpec{

  "The integerGenerator" when {
    "run" should {
      "make random integers" in {
        val test1 = integerGenerator.generate
        assert(test1.isInstanceOf[Int])
      }

      "not return the same in every time" in {
        val test1 = integerGenerator.generate
        val test2 = integerGenerator.generate
        assert(test1 != test2)
      }
    }
  }

  "The booleanGenerator" when {
    "run" should {
      "make random booleans" in {
        val test1 = booleanGenerator.generate
        assert(test1.isInstanceOf[Boolean])
      }

      "have a roughly even distribution" in {
        var trueCount = 0
        var falseCount = 0

        (1 to 1000) map {_ =>
          val b = booleanGenerator.generate
          if(b) trueCount+= 1 else falseCount+= 1
        }

        assert(trueCount > 430)
        assert(falseCount > 430)
      }
    }
  }

}
