/**
 * Created by Katja on 17.4.2015.
 */

import collection._

def getOrderFromIndex(definitionStr: String): Integer = {
  val s = definitionStr.split('|')
  s(0).toInt
}

def countDigraphs(file: String) = {
  scala.io.Source.fromFile(file, "UTF-8").getLines.grouped(2).map(p => getOrderFromIndex(p(0))).toList.groupBy(x => x).mapValues(x => x.length).foreach(d => println(d._1.toString + ":" + d._2.toString))
}

countDigraphs("TwoStarDigraphs1k.txt")