package xyz.discretezoo.core

import externalprocess.{Gap, Lowx}
import xyz.discretezoo.core.polytopes.P2orbit


object Main {

  def main(args: Array[String]): Unit = {

//    val gapProcess = new Gap
//    println(result)
//    println(gapProcess.eval("1+1;"))
//    println(gapProcess.eval("2+2;"))
//    gapProcess.close()

    val P = new P2orbit(3, Set(1))
//    println(P.LOWX.code)
    val lowxOut = P.LOWX.get(40).last
    println(P.GAP.code)
    println(s"index: ${lowxOut._1}")
    println(P.GAP.quotientCode(lowxOut._2.toSet))

//    Range(0, 3).toSet.subsets().filter(s => s.nonEmpty && s.size < 3).foreach(s => {
//      println("\n - - - - - ")
//      println(s)
//      val P2 = new P2orbit(3, s)
//      println(P2.GAP.get)
//      P2.GAP.getIntersectionConditions.foreach(println)
//    })
  }

}
