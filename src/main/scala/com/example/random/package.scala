package com.example

package object random {

  trait Generator[+T] {
    self =>
    def generate: T

    def map[S](f: T => S): Generator[S] = new Generator[S] {
      override def generate: S = f(self.generate)
    }

    def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
      override def generate: S = f(self.generate).generate
    }
  }

  val integerGenerator: Generator[Int] = new Generator[Int] {
    val rand = new java.util.Random

    override def generate: Int = rand.nextInt()
  }

  def integerRangeGenerator(low: Int, high: Int): Generator[Int] = {
    require(high > low)
    for {
      x <- integerGenerator
    } yield (1.0*Math.abs(x)*(high-low)/2147483648.0).toInt + low
  }

  val  doubleGenerator: Generator[Double] = new Generator[Double] {
    val rand = new java.util.Random
    override def generate: Double = rand.nextDouble()
  }

  val charGenerator: Generator[Char] = new Generator[Char] {
    override def generate: Char = {
      val leftLimit = 97 // letter 'a'
      val rightLimit = 122 // letter 'z'
      integerRangeGenerator(leftLimit, rightLimit).generate.toChar
    }
  }

  val stringGenerator: Generator[String] = new Generator[String] {
    override def generate: String = {
      val targetStringLength = integerRangeGenerator(0, 50).generate
      val buffer = new StringBuilder(targetStringLength)
      (0 to targetStringLength) foreach { _ =>
        buffer.append(charGenerator.generate)
      }
      buffer.toString
    }
  }

  val booleanGenerator: Generator[Boolean] = integerGenerator map (_ > 0)

  def pairGenerator[T, U](t: Generator[T], u: Generator[U]): Generator[(T, U)] = for {
    x <- t
    y <- u
  } yield (x, y)

  def single[T](x: T): Generator[T] = new Generator[T] {
    override def generate: T = x
  }

  def oneOfGenerator[T](xs: T*): Generator[Any] = for {
    index <- integerRangeGenerator(0, xs.length)
  } yield xs(index)

}
