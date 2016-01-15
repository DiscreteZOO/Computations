package xyz.discretezoo.core

import java.sql.{DriverManager, Statement}

/**
  * Created by katja on 04/12/15.
  */

class SQLite(dbName: String) {

  private val connection = DriverManager.getConnection(s"jdbc:sqlite:$dbName")
  private val statement: Statement = connection.createStatement()

  statement.setQueryTimeout(30)

  private val results = statement.executeQuery(s"PRAGMA table_info('graph');") // cid, name, type, notnull, dflt_value, pk

//  def getColumns(tableName: String): Seq[SQLiteField] = {
//    var fields = Seq.newBuilder[SQLiteField]
//    val results = statement.executeQuery(s"PRAGMA table_info('$tableName');") // cid, name, type, notnull, dflt_value, pk
//    while(results.next()) {
//      fields += new SQLiteField(results.getInt("cid"), results.getString("name"), SQLiteType.getByName(results.getString("type")), results.getBoolean("notnull"))
//    }
//    fields.result
//  }

//  def getAllRows(tableName: String): Set[Map[Int, Any]] = {
//    val columns = getColumns(tableName)
//    val results = statement.executeQuery(s"SELECT * FROM $tableName;")
//    var rows = Set.newBuilder[Map[Int, Any]]
//    def getValue(field: SQLiteField): Any = {
//      field.sqliteType.name match {
//        case "INTEGER" => results.getInt(field.name)
//        case "REAL" => results.getDouble(field.name)
//        case "TEXT" => results.getString(field.name)
//      }
//    }
//    while(results.next()) {
//      rows += columns.map(column => (column.id, getValue(column))).toMap
//    }
//    rows.result
//  }

  def getSQLiteIterator(tableName: String) = statement.executeQuery(s"SELECT * FROM $tableName;")

}
