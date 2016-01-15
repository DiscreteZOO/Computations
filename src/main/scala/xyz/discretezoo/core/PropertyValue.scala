package xyz.discretezoo.core

/**
  * Created by katja on 03/01/16.
  */
class PropertyValue[T](property: Property[T], value: T) {

  override def toString() = s"${property.name}: $value"

}
