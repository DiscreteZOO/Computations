package xyz.discretezoo.core.externalformats

import xyz.discretezoo.core.graphs.{AdjacencyListBuilder, LabelledVertexNeighbourhood}

import scala.collection.immutable.StringOps

/**
  * Created by katja on 06/12/15.
  *
  * https://cs.anu.edu.au/people/Brendan.McKay/data/formats.txt
  */

class String6(input: String) {

//  TODO drop appropriate number of bytes according to format
//  now parsing sparse6
  val bits = new BitStream(new StringOps(input).getBytes.drop(1).map(b => (b-63).toByte), 2) // skip the ":"
  val n = sizeFromBits
  val vertexBits = getMinPowerOf2

  private def sizeFromBits: Long = {
    var byte = bits.getNextBits(6)
    if (byte < 63) {
      byte
    }
    else {
      byte = bits.getNextBits(6)
      if (byte < 63) byte<<6 | bits.getNextBits(12)
      else bits.getNextBits(36)
    }
  }

  private def getMinPowerOf2: Int = {
    var k = 1
    while (1<<k < n-1) k += 1
    k
  }

  private def parseSparse6: Map[Long, LabelledVertexNeighbourhood] = {

    val adjacencyListBuilder = new AdjacencyListBuilder()

    var v: Long = 0

    while (bits.remainingBits > vertexBits) {

      if (bits.getNextBits(1) > 0) v += 1
      val newVertex = bits.getNextBits(vertexBits)

      if (newVertex > v) v = newVertex
      else adjacencyListBuilder.addEdge(newVertex, v)

    }

    adjacencyListBuilder.getMap

  }

  def parse: Map[Long, LabelledVertexNeighbourhood] = parseSparse6

}
