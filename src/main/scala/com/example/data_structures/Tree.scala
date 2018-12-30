package com.example.data_structures

trait Tree[T]

case class Inner[T](left: Tree[T], right: Tree[T]) extends Tree[T]

case class Leaf[T](v: T) extends Tree[T]

object Tree {

  import com.example.random._

  def LeafGenerator[T](t: Generator[T]): Generator[Leaf[T]] = for {
    v <- t
  } yield Leaf(v)

  def innerTreeGenerator[T](t: Generator[T]): Generator[Inner[T]] = for {
    left <- treeGenerator(t)
    right <- treeGenerator(t)
  } yield Inner(left, right)

  def treeGenerator[T](t: Generator[T]): Generator[Tree[T]] = for {
    isLeaf <- booleanGenerator
    tree <- if (isLeaf) LeafGenerator(t) else innerTreeGenerator(t)
  } yield tree

}
