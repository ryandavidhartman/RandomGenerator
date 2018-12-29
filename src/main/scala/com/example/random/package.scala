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

  val booleanGenerator: Generator[Boolean] = integerGenerator map (_ > 0)

  def pairGenerator[T, U](t: Generator[T], u: Generator[U]): Generator[(T, U)] = for {
    x <- t
    y <- u
  } yield (x, y)

  def single[T](x: T): Generator[T] = new Generator[T] {
    override def generate: T = x
  }

  def integerRangeGenerator(low: Int, high: Int): Generator[Int] = {
    require(high > low)
    for {
      x <- integerGenerator
    } yield (x*(high-low)/2147483648.0).toInt + low + 1
 }

  def oneOfGenerator[T](xs: T*): Generator[Any] = for {
    index <- integerRangeGenerator(0, xs.length)
  } yield xs(index)

}
