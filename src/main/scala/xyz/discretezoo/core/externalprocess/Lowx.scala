package xyz.discretezoo.core.externalprocess

import java.io.File
import scala.math.log10

class Lowx(group: String, maxIndex: Int, normalSubgroups: Boolean = true) extends BatchProcess("lowx")  {

  private val inputFile = inputTempFile()
  parameters = Some(inputFile.getAbsolutePath)
  inputFile.deleteOnExit()

  def runAndGetSubgroups: Iterator[SubgroupData] = {
    val sequences = run().lines.map(_.split(' ')).filter(_.head.forall(_.isDigit))
    sequences.filter(_.head.nonEmpty).map(a => SubgroupData(a.head.toInt, a.tail))
  }

  private def inputTempFile(): File = {
    import java.io.File
    import java.io.PrintWriter
    val inputTempFile = File.createTempFile("discreteZOOtemp", ".lx")
    val printWriter = new PrintWriter(inputTempFile)
    try printWriter.write(generateInput()) finally printWriter close()
    inputTempFile
  }

  private def generateInput(): String = {
    val normalSubgroupsOnly = (if (normalSubgroups) Some("normal_subgroups") else None).toSeq
    (Seq(group) ++ normalSubgroupsOnly ++ Seq("max_index=" + maxIndex.toString)).map(_ + ";").mkString(" ")
  }

  case class SubgroupData(index: Int, generators: Seq[String])

}
