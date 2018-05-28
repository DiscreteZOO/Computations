package xyz.discretezoo.core.externalprocess

import scala.sys.process._

sealed trait ZOOProcess

abstract class BatchProcess(command: String) extends ZOOProcess {

  protected var parameters: Option[String] = None

  def run(): String = Seq(command, parameters.getOrElse("")).mkString(" ").!!

}

abstract class InteractiveProcess(command: String) extends ZOOProcess {

  import scala.concurrent._
  import scala.io._
  import java.io._

  protected val stdin = new SyncVar[OutputStream]
  protected val stdout = new SyncVar[Source]

  protected var process: Option[Process] = None

  reset()

  def reset(): Unit = synchronized {
    close()
    process = Some(Process(command).run(new ProcessIO(
      stdin => InteractiveProcess.this.stdin.put(stdin),
      stdout => InteractiveProcess.this.stdout.put(Source.fromInputStream(stdout, "UTF-8")),
      stderr => Source.fromInputStream(stderr, "UTF-8").getLines.foreach(s => ())
    )))
  }

  def close(): Unit = synchronized {
    if (stdin.isSet) stdin.take()
    if (stdout.isSet) stdout.take()
    process.foreach(_.destroy())
  }

  def eval(cmd: String): String

}
