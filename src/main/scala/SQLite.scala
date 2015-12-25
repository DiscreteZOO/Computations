import java.sql.{DriverManager, Statement}

/**
  * Created by katja on 04/12/15.
  */

class SQLite(dbName: String) {

  val connection = DriverManager.getConnection(s"jdbc:sqlite:$dbName")
  val statement: Statement = connection.createStatement()
  statement.setQueryTimeout(30)

  val graphTableName = "graph"
  val graphColumns = getColumns(graphTableName)
  val graphColumnNamesByIds = graphColumns.map(field => (field.id, field.name)).toMap

  def graphColumnIdToName(id: Int): Option[String] = graphColumnNamesByIds.get(id)

  def getColumns(tableName: String): Seq[SQLiteField] = {
    var fields = Seq.newBuilder[SQLiteField]
    val results = statement.executeQuery(s"PRAGMA table_info('$tableName');") // cid, name, type, notnull, dflt_value, pk
    while(results.next()) {
      fields += new SQLiteField(results.getInt("cid"), results.getString("name"), results.getString("type"), results.getBoolean("notnull"))
    }
    fields.result
  }

  def getAllRows(tableName: String): Set[Map[Int, Any]] = {
    var columns = getColumns(tableName)
    var rows = Set.newBuilder[Map[Int, Any]]
    var results = statement.executeQuery(s"SELECT * FROM $tableName;")
    def getValue(field: SQLiteField): Any = {
      field.fieldType match {
        case "INTEGER" => results.getInt(field.name)
        case "REAL" => results.getDouble(field.name)
        case "TEXT" => results.getString(field.name)
      }
    }
    while(results.next()) {
      rows += columns.map(column => (column.id, getValue(column))).toMap
    }
    rows.result
  }

  def getAllGraphs: Set[Map[String, Any]] = getAllRows(graphTableName).map(m => m.map(f => (graphColumnIdToName(f._1).get, f._2)))

}
