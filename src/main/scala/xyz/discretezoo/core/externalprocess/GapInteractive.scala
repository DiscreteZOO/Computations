package xyz.discretezoo.core.externalprocess

/**
  * Created by katja on 01/05/16.
  */
class GapInteractive extends InteractiveProcess("gap -b -n") {
  // -b disable/enable the banner
  // -n prevent line editing
  stdin.get.write("".getBytes) // set up
  stdin.get.flush()
  println(stdout.get.getLines.next)

  override def eval(cmd: String): String = synchronized {
    stdin.get.write((cmd + "\n").getBytes)
    stdin.get.flush()
    stdout.get.getLines.next.replaceAll("gap> ", "")
    //TODO: deal with errors
  }

  def printToFile(filename: String): String = eval(s"""LogTo("$filename"); Print("$filename");""")
  def endPrintToFile(): String = eval("""LogTo(); Print("stop printing to file");""")
}