import spire.math.Rational

/**
  * Created by katja on 06/12/15.
  */
sealed trait PropertyType[Any] {

  val name: String
  val sqliteName: String

  override def toString = name

}

object PropertyType {

  case object BooleanPropertyType extends PropertyType[Boolean] {
    val name = "Boolean"
    val sqliteName = "INTEGER"
  }

  case object DoublePropertyType extends PropertyType[Double] {
    val name = "Double"
    val sqliteName = "REAL"
  }

  case object IntegerPropertyType extends PropertyType[Int] {
    val name = "Integer"
    val sqliteName = "INTEGER"
  }

  case object RationalPropertyType extends PropertyType[Rational] {
    val name = "Rational"
    val sqliteName = "TEXT"
  }

  case object StringPropertyType extends PropertyType[String] {
    val name = "String"
    val sqliteName = "TEXT"
  }

  def getByName(name: String): PropertyType[_] = {
    name match {
      case "Boolean" => BooleanPropertyType
      case "Double" => DoublePropertyType
      case "Integer" => IntegerPropertyType
      case "Rational" => RationalPropertyType
      case "String" => StringPropertyType
    }
  }

}
