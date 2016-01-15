import java.sql.ResultSet
import io.duality.PersistableSet

/**
  * Created by katja on 03/01/16.
  */
trait ZooObjectStructure[T <: ZooObject] {

  val name: String
  val tableSQLite: String
  val properties: PersistableSet[Property[_]]

  def constructFromSQLite(resultSet: ResultSet): T
  def persistableSet = new PersistableSet[T]

}
