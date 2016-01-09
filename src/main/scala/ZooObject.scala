import io.duality.PersistableSet

/**
  * Created by katja on 05/01/16.
  */
abstract class ZooObject {

//  val uniqueId: String

  val properties = new PersistableSet[PropertyValue[_]]

  def description: String
  override def toString: String

}
