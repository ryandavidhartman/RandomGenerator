package com.example.random

import org.scalatest.WordSpec

class PairGeneratorForExpansionSpecs extends WordSpec {

  def pairGenerator1[T, U](t: Generator[T], u: Generator[U]): Generator[(T, U)] = for {
    x <- t
    y <- u
  } yield (x, y)

  def pairGenerator2[T, U](t: Generator[T], u: Generator[U]): Generator[(T, U)] = {
    t.flatMap{x => for (y <- u) yield (x, y) }
  }

  def pairGenerator3[T, U](t: Generator[T], u: Generator[U]): Generator[(T, U)] = {
    t.flatMap{x => u.map(y => (x, y) ) }
  }

  def pairGenerator4[T, U](t: Generator[T], u: Generator[U]): Generator[(T, U)] = {
    t.flatMap{x => new Generator[(T,U)] {
      override def generate: (T, U) = {
        def f(y:U) = (x,y)
        f(u.generate)
        }
      }
    }
  }

  def pairGenerator5[T, U](t: Generator[T], u: Generator[U]): Generator[(T, U)] = {
    t.flatMap{x => new Generator[(T,U)] {
      override def generate: (T, U) = (x, u.generate)
      }
    }
  }

  def pairGenerator6[T, U](t: Generator[T], u: Generator[U]): Generator[(T, U)] = new Generator[(T,U)] {
    override def generate: (T,U) = {
      def f(x:T): Generator[(T,U)] = new Generator[(T,U)] {
        override def generate: (T, U) = (x, u.generate)
      }
      f(t.generate).generate
    }
  }

  def pairGenerator7[T, U](t: Generator[T], u: Generator[U]): Generator[(T, U)] = new Generator[(T,U)] {
    override def generate: (T,U) = new Generator[(T,U)] {
      override def generate: (T, U) = (t.generate, u.generate)
    }.generate
  }

  def pairGenerator8[T, U](t: Generator[T], u: Generator[U]): Generator[(T, U)] = new Generator[(T,U)] {
    override def generate: (T,U) = (t.generate, u.generate)
  }


  "The pairGenerator1" when {
    "run" should {
      "make random pairs of the correct type" in {
        val (test1, test2) = pairGenerator1(integerGenerator, booleanGenerator).generate
        assert(test1.isInstanceOf[Int])
        assert(test2.isInstanceOf[Boolean])
      }

      "return random values" in {
        val (test1, test2) = pairGenerator1(integerGenerator, integerGenerator).generate
        assert(test1 != test2)

      }
    }
  }

  "The pairGenerator2" when {
    "run" should {
      "make random pairs of the correct type" in {
        val (test1, test2) = pairGenerator2(integerGenerator, booleanGenerator).generate
        assert(test1.isInstanceOf[Int])
        assert(test2.isInstanceOf[Boolean])
      }

      "return random values" in {
        val (test1, test2) = pairGenerator2(integerGenerator, integerGenerator).generate
        assert(test1 != test2)

      }
    }
  }

  "The pairGenerator3" when {
    "run" should {
      "make random pairs of the correct type" in {
        val (test1, test2) = pairGenerator3(integerGenerator, booleanGenerator).generate
        assert(test1.isInstanceOf[Int])
        assert(test2.isInstanceOf[Boolean])
      }

      "return random values" in {
        val (test1, test2) = pairGenerator3(integerGenerator, integerGenerator).generate
        assert(test1 != test2)

      }
    }
  }

  "The pairGenerator4" when {
    "run" should {
      "make random pairs of the correct type" in {
        val (test1, test2) = pairGenerator4(integerGenerator, booleanGenerator).generate
        assert(test1.isInstanceOf[Int])
        assert(test2.isInstanceOf[Boolean])
      }

      "return random values" in {
        val (test1, test2) = pairGenerator4(integerGenerator, integerGenerator).generate
        assert(test1 != test2)

      }
    }
  }

  "The pairGenerator5" when {
    "run" should {
      "make random pairs of the correct type" in {
        val (test1, test2) = pairGenerator5(integerGenerator, booleanGenerator).generate
        assert(test1.isInstanceOf[Int])
        assert(test2.isInstanceOf[Boolean])
      }

      "return random values" in {
        val (test1, test2) = pairGenerator5(integerGenerator, integerGenerator).generate
        assert(test1 != test2)

      }
    }
  }

  "The pairGenerator6" when {
    "run" should {
      "make random pairs of the correct type" in {
        val (test1, test2) = pairGenerator6(integerGenerator, booleanGenerator).generate
        assert(test1.isInstanceOf[Int])
        assert(test2.isInstanceOf[Boolean])
      }

      "return random values" in {
        val (test1, test2) = pairGenerator6(integerGenerator, integerGenerator).generate
        assert(test1 != test2)

      }
    }
  }

  "The pairGenerator7" when {
    "run" should {
      "make random pairs of the correct type" in {
        val (test1, test2) = pairGenerator7(integerGenerator, booleanGenerator).generate
        assert(test1.isInstanceOf[Int])
        assert(test2.isInstanceOf[Boolean])
      }

      "return random values" in {
        val (test1, test2) = pairGenerator7(integerGenerator, integerGenerator).generate
        assert(test1 != test2)

      }
    }
  }

  "The pairGenerator8" when {
    "run" should {
      "make random pairs of the correct type" in {
        val (test1, test2) = pairGenerator8(integerGenerator, booleanGenerator).generate
        assert(test1.isInstanceOf[Int])
        assert(test2.isInstanceOf[Boolean])
      }

      "return random values" in {
        val (test1, test2) = pairGenerator8(integerGenerator, integerGenerator).generate
        assert(test1 != test2)

      }
    }
  }

}
