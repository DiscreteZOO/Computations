package xyz.discretezoo.core

import java.io.File

import externalprocess.{GapBatch, Lowx}
import maniplexes.{M2orbit, ManiplexData}
import xyz.discretezoo.core.GAP.HomomorphismListParser

import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {

    val P = new M2orbit(3, Set(1))

    // Get sample GAP code
//    println(P.GAP.setupForEpimorphismsToGpOrder(10))
//    println(P.GAP.setupForEpimorphismsToGpOrder(50))

    // Get GAP output txts
    // arguments: I, from order, to order
//    computeManiplexes(3, args(0).split(",").map(_.toInt).toSet, args(1).toInt, args(2).toInt)
//    getResources("results").foreach(println)

    val fileIterator = Source.fromFile("test.txt").getLines().mkString
    val test = HomomorphismListParser.deserializeHomomorphism(fileIterator.filter(_ > ' '))
    println(test)
//      new HomomorphismListParser(fileIterator.filter(_ > ' '))
//    test.InputLine.run()

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
      new GapBatch(code, outputFileName).run()
    })
  }

  def getResources(directory: String): Seq[ManiplexData] = {
    val d = new File("results")
    if (d.exists && d.isDirectory)
      d.listFiles.filter(_.isFile).map(file => ManiplexData.fromFileName(file.getName)).toSeq.sorted
    else
      Seq()
  }

}
