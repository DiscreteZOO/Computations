package xyz.discretezoo.core.maniplexes

import xyz.discretezoo.core.maniplexes.M2orbit.M2orbitManiplex

import scala.math.Ordering.Implicits._

case class ManiplexData(rank: Int, I: Set[Int], groupOrder: Int) {
  private def comparable: (Int, Seq[Int], Int) = (rank, I.toSeq.sorted, groupOrder)
  def toM2orbit: M2orbitManiplex = new M2orbitManiplex(rank, I)
  def serialised: String = s"M2.$rank.${M2orbitManiplex.serialiseSymmetryType(I)}.$groupOrder"
}

object ManiplexData {
  implicit def ordering: Ordering[ManiplexData] = Ordering.by(_.comparable)
  def fromFileName(filename: String): ManiplexData = {
    val data = filename.split('.').drop(1).dropRight(1)
    ManiplexData(data(0).toInt, M2orbitManiplex.deserialiseSymmetryType(data(1)), data(2).toInt)
  }
}
