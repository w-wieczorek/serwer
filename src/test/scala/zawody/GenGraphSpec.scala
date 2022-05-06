package zawody

import org.scalatest.funspec.AnyFunSpec
import scalax.collection.immutable
import scalax.collection.GraphPredef._
import scala.collection.mutable

class GenGraphSpec extends AnyFunSpec {
  describe("GenGraph") {
    it("pics randomly an edge indexes") {
      val p = Array.ofDim[Double](4, 4)
      p(0)(1) = 0.0
      p(0)(2) = 0.3
      p(0)(3) = 0.1
      p(1)(2) = 0.0
      p(1)(3) = 0.2
      p(2)(3) = 0.01
      val eopt = GenGraph.roulette(p)
      assert(eopt.isDefined)
      assert(Set((0, 2), (0, 3), (1, 3), (2, 3)) contains eopt.get)
    }
    it("pics randomly an edge according to probabilities") {
      val p = Array.ofDim[Double](4, 4)
      p(0)(1) = 0.01
      p(0)(2) = 0.3
      p(0)(3) = 0.1
      p(1)(2) = 0.0
      p(1)(3) = 0.2
      p(2)(3) = 0.05
      val freq: mutable.HashMap[(Int, Int), Int] = mutable.HashMap.empty
      for(_ <- 1 to 1000) {
        val e = GenGraph.roulette(p).get
        freq.get(e) match {
          case Some(k) => freq.update(e, k + 1)
          case None => freq(e) = 1
        }
      }
      assert(freq((1, 3)) < freq(0, 2))
      assert(freq((0, 3)) < freq(1, 3))
      assert(freq((2, 3)) < freq(0, 3))
      assert(freq((0, 1)) < freq(2, 3))
      assert(!freq.contains((1, 2)))
    }
    it("creates a random graph with nodes of degree 3 to 7") {
      val list = GenGraph.create(32)
      val g = immutable.Graph.from(0 until 32, list.map(p => p._1 ~ p._2))
      val nodes = g.nodes
      nodes.foreach(node => {
        assert(node.degree >= 3 && node.degree <= 7)
      })
    }
    it("creates a larger graph") {
      val list = GenGraph.create(64)
      val g = immutable.Graph.from(0 until 64, list.map(p => p._1 ~ p._2))
      val nodes = g.nodes
      nodes.foreach(node => {
        assert(node.degree >= 3 && node.degree <= 7)
      })
    }
    it("creates a random graph with 100 nodes in an instant") {
      val t1 = System.nanoTime
      val list = GenGraph.create(100)
      val g = immutable.Graph.from(0 until 100, list.map(p => p._1 ~ p._2))
      val duration = (System.nanoTime - t1) / 1e9d
      assert(duration < 1.0)
      val nodes = g.nodes
      nodes.foreach(node => {
        assert(node.degree >= 3 && node.degree <= 7)
      })
    }
  }
}
