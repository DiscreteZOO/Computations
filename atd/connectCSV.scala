/**
 * Created by Katja on 17.4.2015.
 */

import collection._

case class Graph(index: String, canonical: String)
case class Indices(index: String, indexATD: String)

def parseIndices(definitionStr: String): Indices = {
  val s = definitionStr.split('|')
  Indices(s"${s(0)}|${s(1)}", s"${s(0)}|${s(2)}")
}

val digraphs: Iterator[Graph] = scala.io.Source.fromFile("d1000.txt", "UTF-8").getLines.grouped(2).map(p => Graph(p.head, p(1)))
val indices: Iterator[Indices] = scala.io.Source.fromFile("connectATD.txt", "UTF-8").getLines.map(p => parseIndices(p))
val csv: Iterator[String] = scala.io.Source.fromFile("csv.txt", "UTF-8").getLines

val indexMap = mutable.Map.empty[String, String]
val csvMap = mutable.Map.empty[String, String]

while (indices.hasNext) {
  val current = indices.next()
  indexMap += (current.indexATD -> current.index)
}

while (csv.hasNext) {
  val values = csv.next().split(",")
  val newIndex = indexMap.getOrElse(values(0), "")
  if (newIndex.length > 0) {
    csvMap += (newIndex ->  values.drop(1).foldLeft(newIndex)(_ + "," + _))
  }
}

while (digraphs.hasNext) {
  val digraph = digraphs.next()
  val s = digraph.index.split('|')
  println(csvMap(s"${s(0)}|${s(1)}"))
  println(digraph.canonical)
}