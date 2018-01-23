package xyz.discretezoo.core.externalprocess

abstract class BatchProcess(command: String) {

  import scala.sys.process._

  protected var parameters: Option[String] = None

  def run(): String = Seq(command, parameters.getOrElse("")).mkString(" ").!!

}
