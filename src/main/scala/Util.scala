import scala.collection.immutable.StringOps
import scala.collection.mutable
import scala.util.matching.Regex

/**
  * Created by katja on 17/11/15.
  */
object Util {

  def commaJoin(a: Any, b: Any): String = s"$a, $b"

  def parseSparse6(sparse6: String): Unit = { // Map[Int, LabelledVertexNeighbourhood]
    // https://cs.anu.edu.au/people/Brendan.McKay/data/formats.txt

    val input = new StringOps(sparse6)
    val bytes = input.getBytes.drop(1).map(b => (b-63).toByte) // skip the ":"

    def ones(n: Int): Int = (1<<n)-1

    def sumOfBytes(array: Array[Byte]): Long = {

      val bytes = array.map(byte => byte&ones(6))
      val l: Int = array.length-1

      var result: Long = 0

      for (i <- 0 to l) result |= bytes(i).toLong<<(6*(l-i))
      result

    }

    def sizeFromBytes(numberOfBytes: Int): Long = {
      // start at 0 if 1, 1 if 4, 2 if 8
      sumOfBytes(bytes.slice(numberOfBytes >>> 2, numberOfBytes))
    }

    def getSizeBytes: Int = {
      if (bytes(0) < 63) 1
      else if (bytes(0) == 63 && bytes(1) < 63) 4
      else 8
    }

    val n = sizeFromBytes(getSizeBytes)
    val edges = bytes.drop(getSizeBytes)

    def getMinPowerOf2: Int = {
      var k = 1
      while (1<<(k) < n-1) k += 1
      k
    }

    val k = getMinPowerOf2

    def getBi(byte: Byte, startOffset: Int): Boolean = ((byte>>>(7-startOffset)) & ones(1)) > 0

    var currentByte = 0
    var lastByteIndex = 0
    var startOffset = 2

    while (lastByteIndex<edges.length) {
      var result: Long = 0
      var realOffset = startOffset+1
      var remainder = (k-realOffset)%6
      var tailBits = k-(8-realOffset)

      lastByteIndex = currentByte+((k-realOffset)/6)+1

      def firstByte(byte: Byte): Byte = (byte & ones(8-realOffset)).toByte
      def lastByte(byte: Byte): Byte = ((byte>>(6-remainder)) & ones(remainder)).toByte

      result |= firstByte(bytes(currentByte)).toLong<<tailBits
      if (tailBits>1) result += sumOfBytes(bytes.slice(currentByte+1, lastByteIndex))<<remainder

      var Xi = result + lastByte(bytes(lastByteIndex))
      var Bi = getBi(bytes(currentByte), startOffset)

      startOffset = 2+remainder
      currentByte = lastByteIndex
    }

  }

  // TODO requirements on input
  def buildAdjacencyList(adjacencies: Any): Map[Int, LabelledVertexNeighbourhood] = {

    val verticesByIds = mutable.Map[Int, LabelledVertexNeighbourhood]()

    adjacencies match {
      case edgeListString: String => {
        val edgePattern = new Regex("""(\d+)[\ ,]+(\d+)""", "v1", "v2")
        edgePattern.findAllIn(edgeListString).matchData.foreach(edge => {
          addEdge(edge.group("v1").toInt, edge.group("v2").toInt)
        })
      }
//      case edgeListInt:
      case indexMap: Map[Int, Set[Int]] => {
        indexMap.foreach(pair => pair._2.foreach(vertex => addEdge(pair._1, vertex)))
      }
    }

    def addEdge(vertexNo1: Int, vertexNo2: Int): Unit = {
      val vertex1 = verticesByIds.getOrElseUpdate(vertexNo1, new LabelledVertexNeighbourhood(vertexNo1))
      val vertex2 = verticesByIds.getOrElseUpdate(vertexNo2, new LabelledVertexNeighbourhood(vertexNo2))
      vertex1.addNeighbour(vertex2)
      vertex2.addNeighbour(vertex1)
    }

    verticesByIds.values.zipWithIndex.map(_.swap).toMap

  }

}
