package xyz.discretezoo.core.externalprocess

/**
  * Marko Kolar
  */
class Magma extends InteractiveProcess("magma -b") {
  stdin.get.write("SetColumns(0); SetAutoColumns(false); load 'TwoStarDigraph.mgm';\n".getBytes)
  stdin.get.flush()
  stdout.get.getLines.next

  def eval(s: String): String = synchronized {
    stdin.get.write((s + ";\n").getBytes)
    stdin.get.flush()
    stdout.get.getLines.next
  }
}

