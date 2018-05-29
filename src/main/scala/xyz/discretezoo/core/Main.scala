package xyz.discretezoo.core

import java.io.File

import externalprocess.{GapBatch, Lowx}
import maniplexes.{M2orbit, ManiplexData}
import xyz.discretezoo.core.GAP.HomomorphismListParser
import xyz.discretezoo.core.db.ZooDB
import xyz.discretezoo.core.maniplexes.GAP.HomomorphismListInput

import scala.io.Source

object Main {

  val outputDirectory = "results"

  def main(args: Array[String]): Unit = {

    val P = new M2orbit(3, Set(1))
//    println(P.generatorsAllowedToMapToID)

    // Get sample GAP code
//    println(P.GAP.setupForEpimorphismsToGpOrder(10))
//    println(P.GAP.setupForEpimorphismsToGpOrder(50))

    // Get GAP output txts
    // arguments: I, from order, to order
//    computeManiplexes(3, args(0).split(",").map(_.toInt).toSet, args(1).toInt, args(2).toInt)
//    getResources(outputDirectory, (s: String) => s.endsWith(".txt")).foreach(println)
//    ZooDB.createTables()
//    HomomorphismListInput.fileToDB(outputDirectory + "/P2.3.1.10.txt")

//    val fileIterator = Source.fromFile("test.txt").getLines().mkString
//    val test = HomomorphismListParser.deserializeHomomorphism(fileIterator.filter(_ > ' '))
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
    val M = new M2orbit(rank, I)
    Range(from, to + 1).filter(_ % 2 == 0).foreach(gpOrder => {
      println(s"generating order $gpOrder")
      val outputFileName = s"P2.$rank.${I.mkString("-")}.$gpOrder.txt"
      val code = M.GAP.setupForEpimorphismsToGpOrder(gpOrder)
      new GapBatch(code, outputDirectory + "/" + outputFileName).run()
    })
  }

  def getResources(directory: String, filter: Function1[String, Boolean]): Seq[ManiplexData] = {
    val d = new File(outputDirectory)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).map(_.getName).filter(filter)
        .map(fileName => ManiplexData.fromFileName(fileName)).toSeq.sorted
    }
    else {
      Seq()
    }
  }

}
