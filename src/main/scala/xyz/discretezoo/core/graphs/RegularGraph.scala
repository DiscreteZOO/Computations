package xyz.discretezoo.core.graphs

import xyz.discretezoo.core.externalformats.String6

/**
  * Created by katja on 17/11/15.
  */
class RegularGraph(string6: ValidString6, uniqueId: String) extends Graph(string6, uniqueId) {

  require(minDegree == maxDegree, "The input is not a regular graph")

  val degree = minDegree
  override val description = s"An undirected graph of order $order and degree $degree."

}
