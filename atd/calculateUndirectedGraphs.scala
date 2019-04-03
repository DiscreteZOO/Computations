/**
 * Created by Katja on 20.4.2015.
 */

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
    /*stdout.get.getLines.map(line => {
      System.err.println(line)
      line
    }).find(line => line.startsWith("sage: '")).get.drop(7).dropRight(1)*/
    stdout.get.getLines.find(line => line.startsWith("sage: '")).get.drop(7).dropRight(1)
  }
}

val sage = new Sage

case class DigraphIndex(order: Integer, index: Integer)
case class Digraph(index: DigraphIndex, definitionStr: String)
case class DigraphGraph(digraphIndex: DigraphIndex, definitionStr: String)
case class Graph(definitionStr: String)
case class GraphDigraphs(graph: Graph, digraphs: Set[DigraphIndex])

def parseDigraphIndex(dataStr: String): DigraphIndex = {
  val s = dataStr.split('|')
  DigraphIndex(s(0).toInt,s(1).toInt)
}

def canonical(definitionStr: String): String = {
  sage.eval(s"Graph($definitionStr).canonical_label().edges(labels = false)")
}

val sourceFile = args(0)
val inputDigraphs: Iterator[Digraph] = scala.io.Source.fromFile(sourceFile, "UTF-8").getLines.grouped(2).map(p => Digraph(parseDigraphIndex(p(0)), p(1)))
val DigraphGraphs: Iterator[DigraphGraph] = inputDigraphs.map(g => {
  System.err.println(s"Processing ${g.index}")
  DigraphGraph(g.index, canonical(g.definitionStr))
})

import scala.collection.mutable
val digraphsByGraph = new mutable.HashMap[Graph, mutable.Set[DigraphIndex]] with mutable.MultiMap[Graph, DigraphIndex]
var lastOrder: Option[Int] = None


def writeGraph(): Unit = {
  digraphsByGraph.map(p => GraphDigraphs(p._1, p._2.toSet)).zipWithIndex.foreach(g => {
    val graph = g._1.graph.definitionStr
    val indices = g._1.digraphs.map(g => g.order.toString + '|' + g.index.toString).toSeq.sorted.fold("")((a, i) => a + "," + i).drop(1)
    println(indices)
    println(graph.replace("(", "[").replace(")", "]"))
  })
}

while (DigraphGraphs.hasNext) {
  val digraphGraph = DigraphGraphs.next()
  val order = digraphGraph.digraphIndex.order
  if (lastOrder.isEmpty) {
    lastOrder = Some(order)
  } else if (Some(order) != lastOrder) {
    writeGraph()
    lastOrder = Some(order)
    digraphsByGraph.clear()
  }
  digraphsByGraph.addBinding(Graph(digraphGraph.definitionStr), digraphGraph.digraphIndex)
}

writeGraph()

sage.close()
