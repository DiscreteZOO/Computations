/**
 * Created by Katja on 12.5.2015.
 */

import collection._
import java.security.MessageDigest

val md = MessageDigest.getInstance("SHA1")

def valueOf(bytes : Array[Byte]) = bytes.map{ b => String.format("%02X", new java.lang.Integer(b & 0xff)) }.mkString

def sha1(s: String) : String = {
  md.reset
  valueOf(md.digest(s.getBytes))
}

case class Graph(index: String, sha1: String)

val digraphs: Iterator[Graph] = scala.io.Source.fromFile("GWDcanonicalUnd.txt", "UTF-8").getLines.grouped(2).map(p => {
  System.err.println(s"Processing ${p.head}")
  Graph(p.head, sha1(p(1)))
})
val census: Iterator[Graph] = scala.io.Source.fromFile("ERG3k.txt", "UTF-8").getLines.grouped(2).map(p => {
  System.err.println(s"Processing ${p.head}")
  Graph(p.head, sha1(p.head))
})
val censusMap = mutable.Map.empty[String, String]

while (census.hasNext) {
  val digraph = census.next()
  censusMap += (digraph.sha1 -> digraph.index)
}

while (digraphs.hasNext) {
  val digraph = digraphs.next()
  censusMap.get(digraph.sha1).foreach(d => {
    val t = d.split('|')
    println(t(0) + "|" + t(1) + "," + digraph.index)
  })
}