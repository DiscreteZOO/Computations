package xyz.discretezoo.core.externalformats

import java.sql.{DriverManager, Statement}
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
  private val rowIterator = statement.executeQuery(s"SELECT * FROM $table ${join("cvt")} ${join("vt")} ${join("spx")} JOIN object_unique_id ON $table.zooid = object_unique_id.object_id WHERE object_unique_id.algorithm = 'sage';")

  def updateOrders(): Unit = {
    val vtIterator =  statement.executeQuery(s"SELECT * FROM object_unique_id JOIN graph ON graph.zooid = object_id WHERE algorithm = 'sage' AND graph.'name' IS NOT NULL;")
    while (vtIterator.next()) println(s"UPDATE *xyz.discretezoo.core.graphs.Graph* SET *name* = '${vtIterator.getString("name")}' WHERE *uniqueId* = '${vtIterator.getString("unique_id")}';")
  }

  def next = rowIterator.next()
  def get(): T = {
    var connectedness = rowIterator.getInt("average_degree")
    while(connectedness < 1) {
      next
      connectedness = rowIterator.getInt("average_degree")
    }
    zooObjectStructure.constructFromSQLite(rowIterator)
  }

  def close = {
    statement.close()
    connection.close()
  }

  def join(censusName: String) = s"LEFT OUTER JOIN ${table}_$censusName USING (zooid)"

//  graph.zooid as zooid, graph_cvt.cvt_index as "cvt", graph_vt.vt_index as "vt", graph_spx.spx_r as "r", graph_spx.spx_s as "s"


}
