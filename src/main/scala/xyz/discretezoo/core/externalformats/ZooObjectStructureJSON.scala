package xyz.discretezoo.core.externalformats

import upickle.Js
import xyz.discretezoo.core.PropertyType

/**
  * Created by katja on 17/01/16.
  */
class ZooObjectStructureJSON(val name: String, val indices: Seq[String], val skip: Seq[String], val fields: Seq[PropertyJSON]) {

  val primaryKey: String = "zooid"

}

object ZooObjectStructureJSON {
//  implicit val propertyWriter = upickle.default.Writer[Property[_]]{
//    case property => Js.Obj(("name", Js.Str(property.name)), ("propertyType", Js.Str(property.propertyType.name)))
//  }
//  implicit val propertyReader = upickle.default.Reader[Property[_]]{
//    case Js.Str(str) =>
//      val Array(name, propertyTypeName) = str.split(" ")
//      new Property(name, PropertyType.getByName(propertyTypeName))
//  }
}
