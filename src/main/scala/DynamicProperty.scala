/**
  * Created by katja on 05/12/15.
  */
case class DynamicProperty[T](name: String, var value: T) {

  override def toString: String = s"$name: ${value.toString}"

}
