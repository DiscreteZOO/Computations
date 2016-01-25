package xyz.discretezoo.core.externalformats

import java.sql.{DriverManager, Statement}

import io.duality.TransactionManager.atomic
import xyz.discretezoo.core._

/**
  * Created by katja on 08/01/16.
  */
class SQLiteTable[T <: ZooObject](dbName: String, zooObjectStructure: ZooObjectStructure[T]) {

  private val connection = DriverManager.getConnection(s"jdbc:sqlite:$dbName")
  private val statement: Statement = connection.createStatement()

  statement.setQueryTimeout(30)

  private val results = statement.executeQuery(s"PRAGMA table_info('${zooObjectStructure.tableSQLite}');") // cid, name, type, notnull, dflt_value, pk
  private val rowIterator = statement.executeQuery(s"SELECT * FROM ${zooObjectStructure.tableSQLite} JOIN object ON ${zooObjectStructure.tableSQLite}.zooid = object.zooid JOIN graph_cvt ON ${zooObjectStructure.tableSQLite}.zooid = graph_cvt.zooid;")

  def next = rowIterator.next()
  def get(): T = zooObjectStructure.constructFromSQLite(rowIterator)

  def close = {
    statement.close()
    connection.close()
  }

}
