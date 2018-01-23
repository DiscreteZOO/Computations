package xyz.discretezoo.core.externalprocess

/**
  * Created by katja on 01/05/16.
  */
class Gap extends InteractiveProcess("gap -b") {
  stdin.get.write("".getBytes) // set up
  stdin.get.flush()
  stdout.get.getLines.next

  def eval(s: String): String = synchronized {
    stdin.get.write((s + "\n").getBytes)
    stdin.get.flush()
    stdout.get.getLines.next.stripPrefix("gap> ")
  }
}