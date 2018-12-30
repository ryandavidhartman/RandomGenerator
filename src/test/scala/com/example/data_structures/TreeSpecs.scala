package com.example.data_structures

import com.example.random.{integerGenerator, listGenerator}
import org.scalatest.WordSpec

class TreeSpecs extends WordSpec{

  import Tree._

  "The treeGenerator" when {
    "run" should {
      "returns a correct type" in {
        val test1 = treeGenerator(integerGenerator).generate
        assert(test1.isInstanceOf[Tree[Int]])
      }

      "not return the same in every time" in {

        var leafCount = 0
        var innerCount = 0
        val testGenerator = treeGenerator(integerGenerator)

        (1 to 100) foreach{ _ =>
          val x  = testGenerator.generate
          if(x.isInstanceOf[Leaf[Int]])
            innerCount += 1
          else {
            leafCount += 1
          }
        }
        assert(leafCount >= 40)
        assert(leafCount + innerCount == 100)
      }

    }
  }

}
