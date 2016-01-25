package xyz.discretezoo.core

import java.sql.ResultSet
import io.duality.PersistableSet

/**
  * Created by katja on 03/01/16.
  */
trait ZooObjectStructure[T <: ZooObject] {

  val name: String
  val tableSQLite: String

  def constructFromSQLite(resultSet: ResultSet): T

}
