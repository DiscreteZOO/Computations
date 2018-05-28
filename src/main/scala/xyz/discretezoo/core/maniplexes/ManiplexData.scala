package xyz.discretezoo.core.maniplexes

import scala.math.Ordering.Implicits._

case class ManiplexData(rank: Int, I: Set[Int], groupOrder: Int) {
  private def comparable: (Int, Seq[Int], Int) = (rank, I.toSeq.sorted, groupOrder)
  def toM2orbit: M2orbit = new M2orbit(rank, I)
}

object ManiplexData {
  implicit def ordering: Ordering[ManiplexData] = Ordering.by(_.comparable)
  def fromFileName(filename: String): ManiplexData = {
    val data = filename.split('.').drop(1).dropRight(1)
    ManiplexData(data(0).toInt, data(1).split("-").map(_.toInt).toSet, data(2).toInt)
  }
}
