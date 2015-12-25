import io.duality.PersistableSet

/**
  * Created by katja on 21/12/15.
  */
class Contribution {

//  timestamp
//
  val authors = new PersistableSet[String]
  val references = new PersistableSet[String]
  val properties = new PersistableSet[String]
  val objects = new PersistableSet[String]

}
