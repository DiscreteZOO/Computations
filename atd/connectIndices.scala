/**
 * Created by Katja on 15.4.2015.
 */

import collection._

case class Graph(index: String, canonical: String)

val digraphs: Iterator[Graph] = scala.io.Source.fromFile("TwoStarDigraphs1k.txt", "UTF-8").getLines.grouped(2).map(p => Graph(p.head, p(1)))
val census: Iterator[Graph] = scala.io.Source.fromFile("dATD.txt", "UTF-8").getLines.grouped(2).map(p => Graph(p.head, p(1)))
val censusMap = mutable.Map.empty[String, String]

while (census.hasNext) {
  val digraph = census.next()
  censusMap += (digraph.canonical -> digraph.index)
}

while (digraphs.hasNext) {
  val digraph = digraphs.next()
  System.err.println(s"Processing ${digraph.index}")
  val s = digraph.index.split('|')
  val t = censusMap(digraph.canonical).split('|')
  println(s(0) + "|" + s(1) + "|" + t(1))
}