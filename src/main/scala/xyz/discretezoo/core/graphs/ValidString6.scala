package xyz.discretezoo.core.graphs

import xyz.discretezoo.core.externalformats.String6

/**
  * Created by katja on 17/01/16.
  */
class ValidString6(val string: String) {

  require(new String6(string).parse.nonEmpty, "Input must be a valid sparse6 string.")

}
