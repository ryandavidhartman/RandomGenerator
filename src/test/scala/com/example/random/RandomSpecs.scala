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

        (1 to 1000) foreach { _ =>
          val b = booleanGenerator.generate
          if(b) trueCount+= 1 else falseCount+= 1
        }

        assert(trueCount > 430)
        assert(falseCount > 430)
      }
    }
  }

  "The pairGenerator" when {
    "run" should {
      "make random pairs of the correct type" in {
        val (test1, test2) = pairGenerator(integerGenerator, booleanGenerator).generate
        assert(test1.isInstanceOf[Int])
        assert(test2.isInstanceOf[Boolean])
      }

      "return random values" in {
        val (test1, test2) = pairGenerator(integerGenerator, integerGenerator).generate
        assert(test1 != test2)

      }
    }
  }

  "The singleGenerator" when {
    "run" should {
      "return the correct value" in {
        val sentinelValue = 101
        val test1 = single(sentinelValue).generate
        assert(test1.isInstanceOf[Int])
        assert(test1 === sentinelValue)
      }
    }
  }

  "The integerRangeGenerator" when {
    "run" should {
      "make random integers" in {
        val test1 = integerRangeGenerator(0, 100).generate
        assert(test1.isInstanceOf[Int])
      }

      "return for ranges that are non-negative" in {
        val test1 = integerRangeGenerator(0, 100).generate
        val test2 = integerRangeGenerator(100, 150).generate

        assert(test1 >= 0 && test1 <= 100)
        assert(test2 >= 100 && test2 <= 150)
        assert(test1 != test2)
      }

      "return for ranges that are negative" in {
        val test1 = integerRangeGenerator(-100, 0).generate
        val test2 = integerRangeGenerator(-150, -100).generate

        assert(test1 >= -100 && test1 <= 0)
        assert(test2 >= -150 && test2 <= -100)
        assert(test1 != test2)
      }

      "blows up for an invalid range" in {
        assertThrows[IllegalArgumentException]{
          integerRangeGenerator(100, 10)
        }
      }
    }
  }

  "The oneOfGenerator" when {
    "run" should {
      "returns a correct value for variable arguments" in {
        val test1 = oneOfGenerator("red", "blue", "green").generate
        assert(Seq("red", "blue",  "green").contains(test1))
      }

      "returns a correct value for a seq" in {
        val data = Seq("red", "blue",  "green")
        val test1 = oneOfGenerator(data: _*).generate
        assert(data.contains(test1))
      }

    }
  }
}
