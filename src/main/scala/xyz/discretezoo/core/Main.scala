package xyz.discretezoo.core

import java.io.{File, PrintWriter}
import java.util.UUID

import slick.lifted.TableQuery

import scala.io.Source
import xyz.discretezoo.core.externalprocess.{GapBatch, Lowx}
import xyz.discretezoo.core.maniplexes.ManiplexData
import xyz.discretezoo.core.GAP.HomomorphismListParser
import xyz.discretezoo.core.MAGMA.GraphCovers
import xyz.discretezoo.core.db.{Maniplex, Maniplexes, ZooDB}
import xyz.discretezoo.core.maniplexes.GAP.HomomorphismListInput
import xyz.discretezoo.core.maniplexes.M2orbit.M2orbitManiplex
import xyz.discretezoo.core.primitives.ActualPermutation
import xyz.discretezoo.core.util.DZConfig

object Main {

  def main(args: Array[String]): Unit = {

//    ZooDB.getGraphs.foreach(println)

//    val M = new M2orbitManiplex(3, Set(1, 2))
//    M.generatorMap.foreach(println)

//    val M = new M2orbitManiplex(3, Set(0, 1))
//    println(M.generatorMap)
//    M.generatorNames.foreach(println)
//    println(s"""outputFile := IO_File("test.txt", "w");; ${M.GAPCodeSnippets.automorphismGroupsGAP(16)} IO_Close(outputFile);;""")

    // Get GAP output txts
    // arguments: I, from order, to order
//    computeManiplexes(3, M2orbitManiplex.deserialiseSymmetryType(args(0)), args(1).toInt, args(2).toInt)
//    getResources(DZConfig.outputResultsGAP, (s: String) => s.endsWith(".txt")).foreach(println)

    val sets: Seq[Set[Int]] = Seq(Set(0, 2))
//    sets.foreach(s => {
//      val M = new M2orbitManiplex(4, s)
//      println(s, M.generatorMap.size - 1)
//    })
//    sets.foreach(s => computeManiplexes(3, s, 149, 150))
//    computeManiplexes(4, Set(1, 2), 120, 150)

//    println("test")
//    ZooDB.createTables()
//    HomomorphismListInput.fileToDB(DZConfig.outputResultsGAP + "/M2.3.1.2.txt")

    outputFilesToDB(
      DZConfig.outputResultsGAP,
      (s: String) => s.endsWith(".txt"),
      (MD: ManiplexData) => MD.rank == 3)

//    val fileIterator = Source.fromFile("test.txt").getLines().mkString.filter(_ > ' ')
//    val test = HomomorphismListParser.deserializeHomomorphisms(fileIterator)
//    println(test)

    // time lowx
//    val t0 = System.nanoTime()
//    val lowx = new Lowx(P.LOWX.code, 70).runAndGetSubgroups // 1h
//    val t1 = System.nanoTime()
//    println(t1 - t0)

//    makeFlagGraphs()

//    val maniplex = (
//      UUID.fromString("c987bb99-5dbe-4ba5-8b80-d1e62ccd4918"),
//      ManiplexData(3,Set(1),8),
//      List(List(3, 4, 1, 2), List(2, 1, 4, 3), List(3, 2, 1, 4), List(1, 4, 3, 2)))
//
//    val voltages = maniplex._2.toM2orbitManiplex.voltagesMap
//
//    def getListGAP(m: Map[Int, Int]): String = m.map(p => s"[${p._1}, ${p._2}]").mkString("[", ", ", "]")
//
//    val code =
//      s"""getMatching := function(G, element, orbit, label)
//         |  local matching, S, i, v, w, e;
//         |  matching := [];
//         |  S := Set(G);
//         |  for i in [1..Length(S)/2] do
//         |    v := S[1];
//         |    w := v * element;
//         |    Add(matching, [v, w]);
//         |    Remove(S, Position(S, w));
//         |    Remove(S, 1);
//         |  od;
//         |  for e in matching do
//         |    View([[e[1], orbit], [e[2], orbit], label]);
//         |  od;;
//         |end;
//         |
//         |G := Group(${maniplex._3.map(ActualPermutation(_).GAP).mkString(", ")});;
//         |gens := GeneratorsOfGroup(G);
//         |rhos := ${getListGAP(voltages._1)};;
//         |alpha2 := ${getListGAP(voltages._2)};;
//         |alpha3 := ${getListGAP(voltages._3)};;
//         |
//         |for r in rhos do getMatching(G, gens[r[1]], 1, r[2]);; od;;
//         |for a in alpha3 do getMatching(G, gens[a[1]], 2, r[2]);; od;;
//       """.stripMargin
//
//    println(code)



  }

  /* Run with parameters
  - - - - - - - - - - - - - - - -
    Ctrl + Alt + r
    e
    Enter
    Program arguments: Write your command line parameters (space between each item)
    Enter
    */
  def computeManiplexes(rank: Int, I: Set[Int], from: Int, to: Int): Unit = {
    val M = new M2orbitManiplex(rank, I)
    Range(from, to + 1).filter(_ % 2 == 0).foreach(gpOrder => {
      println(s"generating order $gpOrder")
      val mData = ManiplexData(rank, I, gpOrder)
      val outputFileName = s"${mData.serialised}.txt"
      val code = M.GAPCodeSnippets.automorphismGroupsGAP(gpOrder)
      new GapBatch(code, DZConfig.outputResultsGAP + "/" + outputFileName).run()
    })
  }

  def getResources(directory: String, filter: Function1[String, Boolean]): Seq[ManiplexData] = {
    val d = new File(directory)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).map(_.getName).filter(filter)
        .map(fileName => ManiplexData.fromFileName(fileName)).toSeq.sorted
    }
    else {
      Seq()
    }
  }

  def outputFilesToDB(directory: String, fileFilter: Function1[String, Boolean], maniplexFilter: Function1[ManiplexData, Boolean]): Unit = {
    val d = new File(directory)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).map(_.getName).filter(fileFilter)
        .filter(fileName => maniplexFilter(ManiplexData.fromFileName(fileName))).foreach(fileName => {
        println(fileName)
        HomomorphismListInput.fileToDB(s"$directory/$fileName")
      })
    }
  }

  def makeMagmaCode(): Unit = {
    ZooDB.getManiplexes.foreach(m => {
      val fileName = s"${m._2.serialised}.${m._1}"
      val printWriter = new PrintWriter(new File(s"${DZConfig.outputCodeMAGMA}/$fileName.m"))
      val GC = GraphCovers(m._3, M2orbitManiplex.serialiseSymmetryType(m._2.I))
      try printWriter.write(GC.code(s"${DZConfig.outputResultsMAGMA}$fileName.txt")) finally printWriter close()
    })
  }

//  def makeFlagGraphs(): Unit = {
//    ZooDB.getManiplexes.foreach(m => {
//      if (m._2.groupOrder < 10) println(m._3.map())
//    })
//  }

}
