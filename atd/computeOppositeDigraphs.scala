/**
 * Created by Katja on 30.4.2015.
 */

import collection._

abstract class ExternalProcess(command: String) {
  import scala.concurrent._
  import scala.io._
  import scala.sys.process._
  import java.io._

  protected val stdin = new SyncVar[OutputStream]
  protected val stdout = new SyncVar[Source]

  protected var process: Option[Process] = None

  reset()

  def reset(): Unit = synchronized {
    close()
    process = Some(Process(command).run(new ProcessIO(
      stdin => ExternalProcess.this.stdin.put(stdin),
      stdout => ExternalProcess.this.stdout.put(Source.fromInputStream(stdout, "UTF-8")),
      stderr => Source.fromInputStream(stderr, "UTF-8").getLines.foreach(s => ())
    )))
  }

  def close(): Unit = synchronized {
    if (stdin.isSet) stdin.take()
    if (stdout.isSet) stdout.take()
    process.map(_.destroy())
  }
}

class Sage extends ExternalProcess("/home/azarija/sage-6.4.1-x86_64-Linux/sage --nodotsage") {
  var nEvals = 0
  def eval(s: String): String = synchronized {
    if (nEvals >= 100) {
      nEvals = 0
      System.err.println("Resetting Sage")
      reset()
    }
    nEvals += 1
    stdin.get.write(("str(" + s + ")\n").getBytes)
    stdin.get.flush()
    stdout.get.getLines.find(line => line.startsWith("sage: '")).get.drop(7).dropRight(1)
  }
}

case class Digraph(orderIndex: String, definitionStr: String) {
  val order = (orderIndex.split('|'))(0).toInt
}
case class DigraphsByUnderlying(csvStr: String) {
  val digraphslist = csvStr.split(",")
}

def parseDigraphIndex(str: String): String = {
  val s = str.split('|')
  s(0) + "|" + s(1)
}

val sage = new Sage

def canonical(definitionStr: String): String = {
  sage.eval(s"DiGraph($definitionStr).canonical_label().edges(labels = false)")
}

val fileUnderlying = args(0)
val fileDigraphs = args(1)
val indexOfDigraph = mutable.Map.empty[String, String]
val inputDigraphsByUnderlying: Iterator[DigraphsByUnderlying] = scala.io.Source.fromFile(fileUnderlying, "UTF-8").getLines.grouped(2).map(p => DigraphsByUnderlying((p(0).split(';'))(1)))
val inputDigraphs: Iterator[Digraph] = scala.io.Source.fromFile(fileDigraphs, "UTF-8").getLines.grouped(2).map(p => {
  val parsedIndex = parseDigraphIndex(p(0))
  indexOfDigraph += (p(1) -> parsedIndex)
  Digraph(parsedIndex,p(1))
})
val oppositeDigraphsMap = mutable.Map.empty[String, String]

val relatedDigraphIndices = mutable.Map.empty[String, DigraphsByUnderlying]
while (inputDigraphsByUnderlying.hasNext) {
  val current = inputDigraphsByUnderlying.next()
  val digraphsList = current.digraphslist
  digraphsList.map(d => {
    relatedDigraphIndices += (d -> current)
  })
}

val digraphsByRelated = new mutable.HashMap[DigraphsByUnderlying, mutable.Set[Digraph]] with mutable.MultiMap[DigraphsByUnderlying, Digraph]

def recordOppositeDigraphs(): Unit = {
  digraphsByRelated.map(p => {
    System.err.println(p._1)
    p._2.map(d => {
      val reverseDefinition = s"[${d.definitionStr.drop(2).dropRight(2).split("\\], \\[").map(e => e.split(", ")).map(a => s"[${a(1)}, ${a(0)}]").reduceLeft((A, B) => A + ", " + B)}]"
      oppositeDigraphsMap += (d.orderIndex -> indexOfDigraph.get(canonical(reverseDefinition).replace("(", "[").replace(")", "]")).get)
    })
  })
}

var previousOrder: Option[Int] = None

while (inputDigraphs.hasNext) {
  val digraph = inputDigraphs.next()
  val underlying = relatedDigraphIndices.get(digraph.orderIndex).get
  if (previousOrder.isEmpty) {
    previousOrder = Some(digraph.order)
  } else if (Some(digraph.order) != previousOrder) {
    if (underlying.digraphslist.toList.length == 1) {
      oppositeDigraphsMap += (digraph.orderIndex -> digraph.orderIndex)
    }
    else {
      recordOppositeDigraphs()
      digraphsByRelated.clear()
    }
    previousOrder = Some(digraph.order)
    oppositeDigraphsMap.map(f => {
      println(s"${f._1},${f._2}")
    })
    oppositeDigraphsMap.clear()
  }
  digraphsByRelated.addBinding(underlying, digraph)
}
recordOppositeDigraphs()
oppositeDigraphsMap.map(f => {
  println(s"${f._1},${f._2}")
})

sage.close()
