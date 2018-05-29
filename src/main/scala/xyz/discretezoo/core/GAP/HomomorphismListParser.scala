package xyz.discretezoo.core.GAP
/** A class for parsing the GAP homomorphism list output, which is composed of
  * - a list o homomorphisms in [], such that each
  * - homomorphism is a list of generator images in [] for each of the original generators, where each
  * - generator is a permutation in a cycle representation.
  */

import org.parboiled2.{Parser, ParserInput, Rule0, Rule1}
import xyz.discretezoo.core.primitives.{Identity, ActualPermutation, Permutation}

class HomomorphismListParser(val input: ParserInput) extends Parser {
  //    https://github.com/sirthias/parboiled2/#rule-combinators-and-modifiers

  def InputLine: Rule1[Seq[Seq[Option[ActualPermutation]]]] = rule { HomomorphismList ~ EOI }
  def HomomorphismList: Rule1[Seq[Seq[Option[ActualPermutation]]]] = rule { '[' ~ oneOrMore(Homomorphism).separatedBy(',') ~ ']' }

  def Homomorphism: Rule1[Seq[Option[ActualPermutation]]] = rule { '[' ~ oneOrMore(Identity | Cycles).separatedBy(',') ~ ']' }

  def Cycles: Rule1[Option[ActualPermutation]] = rule { oneOrMore(Cycle) ~> ((c: Seq[Seq[Int]]) => Some(ActualPermutation.fromCycleNotation(c))) }
  def Identity: Rule1[Option[ActualPermutation]] = rule { "()" ~ push(None) }

  def Cycle: Rule1[Seq[Int]] = rule { '(' ~ oneOrMore(Label).separatedBy(',') ~ ')' }
  def Label: Rule1[Int] = rule { capture(Digits) ~> ((x: String) => x.toInt) }

  def Digits: Rule0 = rule { oneOrMore(Digit) }
  def Digit: Rule0 = rule { "0" - "9" }

}

object HomomorphismListParser {

  def deserializeHomomorphism(s: String): Seq[Seq[Permutation]] = {
    val input = s.filter(_ > ' ') // filter out whitespace
    val result = new HomomorphismListParser(input).InputLine.run().getOrElse(Seq())
    result.map(homomorphism => homomorphism.map(p => p.getOrElse(Identity)))
  }

}
