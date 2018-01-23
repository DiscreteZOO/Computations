package xyz.discretezoo.core.externalprocess

/**
  * Marko Kolar
  */

class Sage extends InteractiveProcess("/home/azarija/sage-6.4.1-x86_64-Linux/sage --nodotsage") {
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
    stdout.get.getLines.find(line => line.startsWith("sage: '")).get.drop(7).dropRight(1)
  }
}
