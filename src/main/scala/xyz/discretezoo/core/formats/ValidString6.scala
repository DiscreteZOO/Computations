package xyz.discretezoo.core.formats

class ValidString6(val string: String) {

  require(new String6(string).parse.nonEmpty, "Input must be a valid sparse6 string.")

}
