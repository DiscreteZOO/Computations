/**
  * Created by katja on 05/12/15.
  */
case class DatabaseField[T](key: String, var value: Option[T]) {

  override def toString: String = s"$key: ${value.toString}"

}
