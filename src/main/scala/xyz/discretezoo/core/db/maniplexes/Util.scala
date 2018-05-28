package xyz.discretezoo.core.db.maniplexes

import org.parboiled2._
import xyz.discretezoo.core.db.ZooDB
import xyz.discretezoo.core.maniplexes.ManiplexData
import xyz.discretezoo.core.primitives.Permutation

import scala.io.Source

object Util {



  /* Store GAP output to database
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

  private def deserializeHomomorphism(s: String): Seq[Seq[String]] = {
    val input = s.filter(_ > ' ') // filter out whitespace
    new GAPHomomorphismList(input).InputLine.run()
    Seq()
  }

  private def parseHomomorphismList(data: ManiplexData, s: String, smallGroupId: Int) = {
    val M = data.toM2orbit
    // TODO test if all generators are mapped ok
    deserializeHomomorphism(s).filter(_.zipWithIndex.filter(t => {
      !M.generatorsAllowedToMapToID.contains(t._2 + 1)
    }).forall(t => t._1 != "()")).map(images => {
      Maniplex(
        uuid            = None,
        id              = 0,
        rank            = data.rank,
        symmetryType    = data.I.mkString(","),
        smallGroupOrder = data.groupOrder,
        smallGroupId    = smallGroupId,
        generators = List(),
        flagGraph       = "",
        underlyingGraph = ""
      )
    })
  }

  class GAPHomomorphismList(val input: ParserInput) extends Parser {
//    https://github.com/sirthias/parboiled2/#rule-combinators-and-modifiers

    def InputLine: Rule1[Seq[Seq[Option[Permutation]]]] = rule { HomomorphismList ~ EOI }
    def HomomorphismList: Rule1[Seq[Seq[Option[Permutation]]]] = rule { '[' ~ oneOrMore(Homomorphism).separatedBy(',') ~ ']' }

    def Homomorphism: Rule1[Seq[Option[Permutation]]] = rule { '[' ~ oneOrMore(Identity | Cycles).separatedBy(',') ~ ']' }

    def Cycles: Rule1[Option[Permutation]] = rule { oneOrMore(Cycle) ~> ((c: Seq[Seq[Int]]) => Some(Permutation.fromCycleNotation(c))) }
    def Identity: Rule1[Option[Permutation]] = rule { "()" ~ push(None) }

    def Cycle: Rule1[Seq[Int]] = rule { '(' ~ oneOrMore(Label).separatedBy(',') ~ ')' }
    def Label: Rule1[Int] = rule { capture(Digits) ~> ((x: String) => x.toInt) }

    def Digits: Rule0 = rule { oneOrMore(Digit) }
    def Digit: Rule0 = rule { "0" - "9" }

  }


}