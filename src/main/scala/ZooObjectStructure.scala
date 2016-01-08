import io.duality.PersistableSet

/**
  * Created by katja on 03/01/16.
  */
trait ZooObjectStructure {

  val name: String
  val dynamicProperties: PersistableSet[DynamicProperty[Any]]

}
