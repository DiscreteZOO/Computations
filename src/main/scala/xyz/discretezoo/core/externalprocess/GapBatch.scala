package xyz.discretezoo.core.externalprocess

import java.io.{File, PrintWriter}

class GapBatch(code: String, outputFileName: String) extends BatchProcess("gap -b -n -q") {

  private val inputFile = inputTempFile()
  parameters = Some(inputFile.getAbsolutePath)
  inputFile.deleteOnExit()

  private def inputTempFile(): File = {
    val inputTempFile = File.createTempFile("discreteZOOtemp", ".g")
    val printWriter = new PrintWriter(inputTempFile)
    try printWriter.write(s"""OutputLogTo("$outputFileName");; $code OutputLogTo();;""") finally printWriter close()
    inputTempFile
  }

}