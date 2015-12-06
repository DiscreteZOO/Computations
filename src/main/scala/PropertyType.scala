/**
  * Created by katja on 06/12/15.
  */
sealed trait PropertyType

object PropertyType {

  case object BooleanPropertyType extends PropertyType
  case object DoublePropertyType extends PropertyType
  case object IntegerPropertyType extends PropertyType
  case object StringPropertyType extends PropertyType

}
