package xyz.discretezoo.core.maniplexes.GAP

import java.util.UUID

import xyz.discretezoo.core.GAP.HomomorphismListParser._
import xyz.discretezoo.core.db.{Maniplex, ZooDB}
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

    for (line <- fileIterator) {
      // if current line is a number, store the collected lines
      if (line.forall(_.isDigit)) {
        if (smallGroupId != 0) ZooDB.insertManiplexes(parseHomomorphismList(data, collector, smallGroupId))
        collector = ""
        smallGroupId = line.toInt
      }
      else {
        collector += line
      }
    }
  }

  /** Produces a sequence of database maniplex objects from the parsed GAP output */
  private def parseHomomorphismList(data: ManiplexData, s: String, smallGroupId: Int) = {

    /** Checks if the homomorphism produces a degenerate maniplex */
    def isValidHomomorphism(homomorphism: Seq[Permutation]): Boolean = {
      val M = data.toM2orbit
      // TODO test if all generators are mapped ok
      homomorphism.zipWithIndex.filter(t => { // zips each generator image with its index
        !M.generatorsAllowedToMapToID.contains(t._2 + 1) // selects those that must not be identity
      }).forall(t => t._1 != Identity) //none of the selected can be identity
    }

    val homomorphismList = deserializeHomomorphism(s)
    val groupDegree = homomorphismList.map(homomorphism => homomorphism.map(_.max).max).max // largest point moved
    homomorphismList.filter(isValidHomomorphism).map(images => {
      Maniplex(
        uuid            = UUID.randomUUID(),
        rank            = data.rank,
        symmetryType    = data.I.mkString(","),
        smallGroupOrder = data.groupOrder,
        smallGroupId    = smallGroupId,
        generators      = images.map(_.ofDegree(groupDegree).permutation.toList).toList,
        flagGraph       = "",
        underlyingGraph = ""
      )
    })
  }

}