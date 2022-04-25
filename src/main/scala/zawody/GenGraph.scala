package zawody

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable
import scala.collection.mutable
import scalax.collection.GraphPredef._

import scala.util.Random

/*
Construction (loosely) based on the paper: A Sequential Algorithm for Generating
Random Graphs by Mohsen Bayati et al. (https://web.stanford.edu/~saberi/sis2.pdf)
*/
object GenGraph {
  def roulette(arr: Array[Array[Double]]): Option[(Int, Int)] = {
    var aSum = 0.0
    val n = arr.length
    for(i <- 0 until n - 1; j <- i + 1 until n) {
      aSum += arr(i)(j)
    }
    if(aSum > 0.0) {
      var s = 0.0
      val r = Random.nextDouble()
      var i = -1
      var j = 0
      while(s == 0.0) {
        i += 1
        assert(i < n - 1)
        j = i
        while(s == 0.0 && j < n - 1) {
          j += 1
          s += arr(i)(j)
        }
      }
      while(r > s/aSum) {
        if(j + 1 == n) {
          i += 1
          j = i
        }
        j += 1
        s += arr(i)(j)
      }
      assert(i >= 0 && i < j && j < n)
      Option((i, j))
    } else {
      None
    }
  }

  def create(nodeSize: Int): List[(Int, Int)] = {
    assert(nodeSize >= 32)
    var built = false
    val edges: mutable.HashSet[(Int, Int)] = mutable.HashSet.empty
    val d = new Array[Int](nodeSize)
    val p = Array.ofDim[Double](nodeSize, nodeSize)
    var g: immutable.Graph[Int, UnDiEdge] = null
    while(!built) {
      edges.clear()
      (0 until nodeSize).foreach(idx => d(idx) = Random.nextInt(6) + 2)
      val m = d.sum/2
      var e: Option[(Int, Int)] = None
      do {
        for(i <- 0 until nodeSize - 1; j <- i + 1 until nodeSize) {
          if(edges.contains(i, j)) {
            p(i)(j) = 0.0
          } else {
            p(i)(j) = d(i)*d(j)*(1.0 - (d(i)*d(j))/(4.0*m))
          }
        }
        e = roulette(p)
        if(e.isDefined) {
          edges.add(e.get)
          d(e.get._1) -= 1
          d(e.get._2) -= 1
        }
      } while(e.isDefined)
      built = (edges.size >= m && edges.size <= 5*m/2)
      if(built) {
        g = immutable.Graph.from(0 until nodeSize, edges.map(p => p._1 ~ p._2))
        built = g.isConnected
      }
    }
    edges.toList
  }
}
