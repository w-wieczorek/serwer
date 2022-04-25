package zawody

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.mutable
import scalax.collection.GraphPredef._

object Rules {
  def makeMove(token: Int, nextNode: Int, graph: mutable.Graph[Int, UnDiEdge]): Boolean = {
    graph.find(token ~ nextNode) match {
      case Some(e) => { graph -= e; true }
      case None => false
    }
  }
  def possibleMoves(token: Int, graph: mutable.Graph[Int, UnDiEdge]): Int = {
    graph.get(token).degree
  }
}
