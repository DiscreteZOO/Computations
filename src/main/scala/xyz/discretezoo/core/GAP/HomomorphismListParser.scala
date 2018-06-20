package xyz.discretezoo.core.GAP
/** A class for parsing the GAP homomorphism list output, which is composed of
  * - a list o homomorphisms in [], such that each
  * - homomorphism is a list of generator images in [] for each of the original generators, where each
  * - generator is a permutation in a cycle representation.
  */

import org.parboiled2.{Rule0, _}
import xyz.discretezoo.core.primitives.{ActualPermutation, Identity, Permutation}

class HomomorphismListParser(val input: ParserInput) extends Parser {
  //    https://github.com/sirthias/parboiled2/#rule-combinators-and-modifiers

  def InputLine: Rule1[Seq[HomomorphismWithProperties]] = rule { HomomorphismList ~ EOI }
  def HomomorphismList: Rule1[Seq[HomomorphismWithProperties]] = rule { oneOrMore(HomomorphismData) }
  def HomomorphismData: Rule1[HomomorphismWithProperties] =
    rule { '[' ~ '[' ~ Permutations ~ ']'  ~ ',' ~ '[' ~ PropertyList ~ ']' ~ ']' ~> HomomorphismWithProperties }

  // Permutations
  def Permutations: Rule1[Seq[Permutation]] = rule { oneOrMore(Cycles | EmptyCycle).separatedBy(',') }
  def Cycles: Rule1[Permutation] =
    rule { oneOrMore(Cycle) ~> ((c: Seq[Seq[Int]]) => ActualPermutation.fromCycleNotation(c)) }
  def EmptyCycle: Rule1[Permutation] = rule { "()" ~ push(Identity) }
  def Cycle: Rule1[Seq[Int]] = rule { '(' ~ oneOrMore(Label).separatedBy(',') ~ ')' }
  def Label: Rule1[Int] = rule { capture(oneOrMore(CharPredicate.Digit)) ~> ((x: String) => x.toInt) }

  // Properties
  def PropertyList: Rule1[Seq[HomomorphismProperty]] = rule { oneOrMore(Property).separatedBy(',') }
  def Property: Rule1[HomomorphismProperty] =
    rule { '[' ~ PropertyName ~ ',' ~ PropertyValue ~ ']' ~> HomomorphismProperty }
  def PropertyName: Rule1[String] = rule { '"' ~ capture(oneOrMore(CharPredicate.LowerAlpha)) ~ '"' }
  def PropertyValue: Rule1[String] = rule { PropertySingleValue | PropertyListValue }
  def PropertyListValue: Rule1[String] = rule { '[' ~ capture(zeroOrMore(SingleValue).separatedBy(',')) ~ ']' }
  def PropertySingleValue: Rule1[String] = rule { capture(SingleValue) }
  def SingleValue: Rule0 = rule { oneOrMore(CharPredicate.AlphaNum) }

}

object HomomorphismListParser {

  def deserializeHomomorphisms(s: String): Seq[HomomorphismWithProperties] = {
    val input = s.filter(_ > ' ') // filter out whitespace
    new HomomorphismListParser(input).InputLine.run().getOrElse(Seq())
  }

  def deserializeTest(s: String): Seq[HomomorphismWithProperties] = {
    val input = s.filter(_ > ' ') // filter out whitespace
    new HomomorphismListParser(input).HomomorphismList.run().get
  }

}