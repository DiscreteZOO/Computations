package xyz.discretezoo.core.maniplexes.GAP

import java.util.UUID

import xyz.discretezoo.core.GAP.HomomorphismListParser._
import xyz.discretezoo.core.GAP.HomomorphismWithProperties
import xyz.discretezoo.core.db.{Maniplex, ZooDB}
import xyz.discretezoo.core.maniplexes.M2orbit.M2orbitManiplex
import xyz.discretezoo.core.maniplexes.ManiplexData
import xyz.discretezoo.core.primitives.{Identity, Permutation}

import scala.io.Source

/** Uses the GAP homomorphism list parser to obtain maniplex data from GAP output.
  * - Filters out degenerate homomorphisms.
  * - Stores into database.
  */
object HomomorphismListInput {

  /** Store GAP output to database.
    * Reads the file, collects the homomorphism list lines by group and passes them to the parser
    */
  def fileToDB(outputFileName: String): Unit = {
    val fileIterator = Source.fromFile(outputFileName).getLines()
    val data = ManiplexData.fromFileName(outputFileName)

    var smallGroupId = 0
    var collector = ""

    def insert(): Unit = ZooDB.insertManiplexes(parseHomomorphismList(data, collector, smallGroupId))

    for (line <- fileIterator) {
      // if current line is a number, store the collected lines
      if (line.nonEmpty) {
        if (line.forall(_.isDigit)) {
          if (smallGroupId != 0 && collector.nonEmpty) insert()
          collector = ""
          smallGroupId = line.toInt
        }
        else {
          collector += line
        }
      }
    }
    // store any remaining collected lines
    if (collector.nonEmpty) insert()
  }

  /** Produces a sequence of database maniplex objects from the parsed GAP output */
  private def parseHomomorphismList(data: ManiplexData, s: String, smallGroupId: Int): Seq[Maniplex] = {

    val homomorphismList = deserializeHomomorphisms(s)
    val groupDegree = homomorphismList.map(homomorphism => homomorphism.homomorphism.map(_.max).max).max // largest point moved
    homomorphismList.map(hp => {
      def booleanPropertyValue(name: String): Boolean = hp.properties.filter(_.name == name).map(_.value == "true").head
      Maniplex(
        uuid = UUID.randomUUID(),
        flagGraph = "",
        generators = hp.homomorphism.map(_.ofDegree(groupDegree).permutation.toList).toList,
        orbits = hp.orbits,
        rank = data.rank,
        isPolytope = booleanPropertyValue("polytope"),
        isRegular = booleanPropertyValue("regular"),
        smallGroupId = smallGroupId,
        smallGroupOrder = data.groupOrder,
        symmetryType = M2orbitManiplex.serialiseSymmetryType(data.I), underlyingGraph = "")
    })
  }

}