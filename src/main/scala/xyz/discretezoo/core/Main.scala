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
import xyz.discretezoo.core.util.DZConfig

object Main {

  def main(args: Array[String]): Unit = {

    val M = new M2orbitManiplex(3, Set(1))
    println(s"""outputFile := IO_File("test.108.txt", "w");; ${M.codeSnippetsGAP.automorphismGroupsGAP(108)} IO_Close(outputFile);;""")

    // TODO: external/gap/improved_gquotient.g in GAP code

    // Get GAP output txts
    // arguments: I, from order, to order
//    computeManiplexes(3, M2orbit.deserialiseSymmetryType(args(0)), args(1).toInt, args(2).toInt)
//    getResources(outputDirectory, (s: String) => s.endsWith(".txt")).foreach(println)

//    ZooDB.createTables()
//    HomomorphismListInput.fileToDB("maniplexes/gap/output/M2.3.1.18.txt")

//    val fileIterator = Source.fromFile("test1.txt").getLines().mkString.filter(_ > ' ')
//    val test = HomomorphismListParser.deserializeTest(fileIterator)
//    println(test)

    // time lowx
//    val t0 = System.nanoTime()
//    val lowx = new Lowx(P.LOWX.code, 70).runAndGetSubgroups // 1h
//    val t1 = System.nanoTime()
//    println(t1 - t0)

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
      val code = M.codeSnippetsGAP.automorphismGroupsGAP(gpOrder)
      new GapBatch(code, DZConfig.outputResultsGAP + "/" + outputFileName).run()
    })
  }

  def getResources(directory: String, filter: Function1[String, Boolean]): Seq[ManiplexData] = {
    val d = new File(DZConfig.outputResultsGAP)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).map(_.getName).filter(filter)
        .map(fileName => ManiplexData.fromFileName(fileName)).toSeq.sorted
    }
    else {
      Seq()
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

}
