package xyz.discretezoo.core

/**
  * Created by katja on 05/12/15.
  */
case class Property[T](name: String, propertyType: PropertyType[T]) {

  override def toString(): String = s"$name ($propertyType)"

}
