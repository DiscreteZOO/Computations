import java.sql.ResultSet
import io.duality.PersistableSet

/**
  * Created by katja on 03/01/16.
  */
abstract class ZooObjectStructure[T <: ZooObject] {

  val name: String
  val tableSQLite: String
  lazy val properties = InitFileParser.getProperties(name)

  def constructFromSQLite(resultSet: ResultSet): T
  def persistableSet = new PersistableSet[T]

}
