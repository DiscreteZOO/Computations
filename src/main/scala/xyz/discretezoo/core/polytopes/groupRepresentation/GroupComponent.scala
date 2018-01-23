package xyz.discretezoo.core.polytopes.groupRepresentation

import scala.annotation.tailrec

// Generators and Relators
sealed trait GroupComponent

/*
 * Standard generators, indexed by the flag rank-involutions
 * i and j commute if |i-j| > 1
 */

case class Generator(subscript: Seq[Int]) extends GroupComponent {

  def isTrivial: Boolean = subscript.isEmpty
  def reverse: Generator = Generator(subscript.reverse)
  def simplified: Generator = Generator(simplify(subscript))
  def inverse: Relator = Relator(this, -1)
  def +(that: Generator): Relator = Relator(this, that)

  @tailrec
  private final def simplify(seq: Seq[Int], accumulated: Seq[Int] = Seq()): Seq[Int] = {
    seq match {
      case Nil => accumulated
      case first :: rest =>
        val (newSeq, newAccumulated) = rest.span(_ != first) match {
          case (matched, _::t) =>
            if (matched.forall(i => Math.abs(i - first) > 1)) (matched ++ t, accumulated)
            else (rest, accumulated :+ first)
          case _ => (rest, accumulated :+ first)
        }
        simplify(newSeq, newAccumulated)
    }
  }

}

object Generator {
  def apply(s1: Int): Generator = new Generator(Seq(s1))
  def apply(s1: Int, s2: Int): Generator = new Generator(Seq(s1, s2))
  def apply(s1: Int, s2: Int, s3: Int): Generator = new Generator(Seq(s1, s2, s3))
  def apply(t2: (Int, Int)): Generator = Generator(t2._1, t2._2)
  def apply(t3: (Int, Int, Int)): Generator = Generator(t3._1, t3._2, t3._3)
}

/*
 * Relators
 */

case class Relator(relation: Seq[(Generator, Int)]) extends GroupComponent {
  def inverse: Relator = Relator(relation.map(p => (p._1, -1 * p._2)).reverse)
  def ++(that: Relator): Relator = Relator(relation ++ that.relation)
  def +(that: Generator): Relator = this ++ Relator(that)
}

object Relator {
  def apply(generator: Generator, power: Int): Relator = new Relator(Seq((generator, power)))
  def apply(g1: Generator): Relator = new Relator(Seq((g1, 1)))
  def apply(g1: Generator, g2: Generator): Relator = new Relator(Seq((g1, 1), (g2, 1)))
  def apply(g1: Generator, g2: Generator, g3: Generator): Relator = new Relator(Seq((g1, 1), (g2, 1), (g3, 1)))
}