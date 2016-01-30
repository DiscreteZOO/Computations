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
  private val table = zooObjectStructure.tableSQLite

  statement.setQueryTimeout(30)

  private val results = statement.executeQuery(s"PRAGMA table_info('$table');") // cid, name, type, notnull, dflt_value, pk
  private val rowIterator = statement.executeQuery(s"SELECT * FROM $table JOIN graph_cvt ON $table.zooid = graph_cvt.zooid JOIN object_unique_id ON $table.zooid = object_unique_id.object_id WHERE algorithm = 'sage' AND graph.zooid > 60500;")

  def next = rowIterator.next()
  def get(): T = zooObjectStructure.constructFromSQLite(rowIterator)

  def close = {
    statement.close()
    connection.close()
  }

}
