package xyz.discretezoo.core.externalprocess

import java.io.{File, PrintWriter}

class GapBatch(code: String, outputFileName: String) //, memory: String
  extends BatchProcess(s"gap -b -n -q") { // -o $memory

  private val inputFile = inputTempFile()
  parameters = Some(inputFile.getAbsolutePath)
  inputFile.deleteOnExit()

  private def inputTempFile(): File = {
    val inputTempFile = File.createTempFile("discreteZOOtemp", ".g")
    val printWriter = new PrintWriter(inputTempFile)
    try printWriter.write(s"""outputFile := IO_File("$outputFileName", "w");; $code IO_Close(outputFile);;""")
    finally printWriter close()
    inputTempFile
  }

}