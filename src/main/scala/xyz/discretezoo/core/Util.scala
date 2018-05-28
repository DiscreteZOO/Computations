package xyz.discretezoo.core

import scala.annotation.tailrec

object Util {

  @tailrec
  def sliceConsecutive[T](seq: Seq[T], accumulated: Seq[Seq[T]] = Seq()): Seq[Seq[T]] = {
    seq match {
      case Nil => accumulated
      case first :: rest =>
        val (head, tail) = seq.span(_ == first)
        sliceConsecutive(tail, accumulated :+ head)
    }
  }

  def countConsecutive[T](seq: Seq[T]): Seq[Tuple2[T, Int]] = {
    sliceConsecutive(seq).map(segment => Tuple2(segment.head, segment.length))
  }

}
