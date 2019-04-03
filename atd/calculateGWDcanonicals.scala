/**
 * Created by Katja on 4.5.2015.
 */

abstract class ExternalProcess(command: String) {
  import scala.concurrent._
  import scala.io._
  import scala.sys.process._
  import java.io._

  protected val stdin = new SyncVar[OutputStream]
  protected val stdout = new SyncVar[Source]

  protected val process = Process(command).run(new ProcessIO(
    stdin => ExternalProcess.this.stdin.put(stdin),
    stdout => ExternalProcess.this.stdout.put(Source.fromInputStream(stdout, "UTF-8")),
    stderr => Source.fromInputStream(stderr, "UTF-8").getLines.foreach(s => ())
  ))

  def close(): Unit = synchronized {
    stdin.get.close()
    stdout.get.close()
    process.destroy()
  }
}

class Sage extends ExternalProcess("/home/azarija/sage-6.4.1-x86_64-Linux/sage") {
  def eval(s: String): String = synchronized {
    stdin.get.write(("str(" + s + ")\n").getBytes)
    stdin.get.flush()
    stdout.get.getLines.find(line => line.startsWith("sage: '")).get.drop(7).dropRight(1)
  }
}

case class Graph(indexStr: String, definitionStr: String)

def canonical(graphDefinitionStr: String): String = {
  sage.eval(s"DiGraph([$graphDefinitionStr]).canonical_label().edges(labels = false)")
  //sage.eval(s"Graph([$graphDefinitionStr]).canonical_label().edges(labels = false)")
}

val sage = new Sage

val file = args(0)
val inputGraphs: Iterator[Graph] = scala.io.Source.fromFile(file, "UTF-8").getLines.grouped(2).map(p => Graph(p(0), canonical(p(1).drop(2).dropRight(2))))

while (inputGraphs.hasNext) {
  val graph = inputGraphs.next()
  System.err.println(s"Processing ${graph.indexStr}")
  println(graph.indexStr)
  println(graph.definitionStr)
}

sage.close()


