package xyz.discretezoo.core

import spire.math.Rational

/**
  * Created by katja on 03/01/16.
  */
trait PropertyValue[T] {

  val property: Property[T]
  val value: T

  override def toString() = s"${property.name}: $value"

}

case class BooleanPropertyValue(property: Property[Boolean], value: Boolean) extends PropertyValue[Boolean]
case class DoublePropertyValue(property: Property[Double], value: Double) extends PropertyValue[Double]
case class IntPropertyValue(property: Property[Int], value: Int) extends PropertyValue[Int]
case class RationalPropertyValue(property: Property[Rational], value: Rational) extends PropertyValue[Rational]
case class StringPropertyValue(property: Property[String], value: String) extends PropertyValue[String]

