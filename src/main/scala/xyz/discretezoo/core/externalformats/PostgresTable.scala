package xyz.discretezoo.core.externalformats

import java.sql.{ResultSet, Statement, DriverManager}

/**
  * Created by katja on 25/01/16.
  */
class PostgresTable(jdbcConnectionString: String) {

  def countPerOrder(): String = {

    val pgsql = DriverManager.getConnection(jdbcConnectionString)
    val query: Statement = pgsql.createStatement()
    val tableName = quotedIdentifier("xyz.discretezoo.core.graphs.Graph")
    val rowIterator = query.executeQuery(s"SELECT $tableName.order as order, COUNT(*) as number FROM $tableName GROUP BY $tableName.order ORDER BY $tableName.order ASC;")
    val result = StringBuilder.newBuilder

    while (rowIterator.next()) result.append(s"${rowIterator.getInt(1)}\t${rowIterator.getInt(2)}\n")

    query.close()
    pgsql.close()

    s"[${result.toString.drop(1)}]"

  }

  def filter(propertiesString: String): String = {

    val pgsql = DriverManager.getConnection(jdbcConnectionString)
    val query: Statement = pgsql.createStatement()
    val tableName = quotedIdentifier("xyz.discretezoo.core.graphs.Graph")
    val rowIterator = query.executeQuery(s"SELECT * FROM $tableName${constructWhereClause(propertiesString)};")
    val result = StringBuilder.newBuilder

    while (rowIterator.next()) result.append(s",${rowToJSON(rowIterator)}")

    query.close()
    pgsql.close()

    s"[${result.toString.drop(1)}]"

  }

  def download(propertiesString: String, mode: String): String = {

    val pgsql = DriverManager.getConnection(jdbcConnectionString)
    val query: Statement = pgsql.createStatement()
    val tableName = quotedIdentifier("xyz.discretezoo.core.graphs.Graph")
    val tableString6 = quotedIdentifier("xyz.discretezoo.core.graphs.ValidString6")
    val result = StringBuilder.newBuilder

    if (mode == "package") {
      result.append("import discretezoo.entities.cvt\nset = [discretezoo.entities.cvt.CVTGraph(x) for x in [\n")
      val rowIterator = query.executeQuery(s"SELECT ${quotedIdentifier("uniqueId")} FROM $tableName${constructWhereClause(propertiesString)};")
      while (rowIterator.next()) result.append(s"\t'${rowIterator.getString("uniqueId")}',\n")
      result.dropRight(1).append("]]\n")
    }
    else {
      val rowIterator = query.executeQuery(s"SELECT string FROM $tableName JOIN $tableString6 ON $tableName.string6 = $tableString6.${quotedIdentifier("@id")}${constructWhereClause(propertiesString)};")
      while (rowIterator.next()) result.append(s"${rowIterator.getString("string")}\n")
    }

    query.close()
    pgsql.close()

    result.toString

  }

  def count(propertiesString: String): String = {

    val pgsql = DriverManager.getConnection(jdbcConnectionString)
    val query: Statement = pgsql.createStatement()
    val rowIterator = query.executeQuery(s"SELECT COUNT(*) FROM ${quotedIdentifier("xyz.discretezoo.core.graphs.Graph")}${constructWhereClause(propertiesString)};")

    rowIterator.next()
    val result = rowIterator.getInt(1)

    query.close()
    pgsql.close()

    s"${result}"

  }

  private def rowToJSON(resultSet: ResultSet): String = {
    val booleans = Seq("isArcTransitive", "isBipartite", "isCayley", "isDistanceRegular", "isDistanceTransitive", "isEdgeTransitive", "isMoebiusLadder", "isPartialCube", "isPrism", "isSplit", "isStronglyRegular", "isSpx")
    val integers = Seq("cliqueNumber", "diameter", "girth", "oddGirth", "order", "trianglesCount", "symcubicIndex", "cvtIndex", "vtIndex")
    val intermediate = booleans.map(property => {
      s"${quotedIdentifier(camelToUnderscores(property))}: ${resultSet.getBoolean(property).toString}"
    }).foldLeft(s"${quotedIdentifier("id")}: ${quotedIdentifier(resultSet.getString("uniqueId"))}")(commaJoin)
    val jsonString = integers.map(property => s"${quotedIdentifier(camelToUnderscores(property))}: ${resultSet.getInt(property).toString}").foldLeft(intermediate)(commaJoin)
    s"{ $jsonString }"
  }

  private def constructWhereClause(propertiesString: String): String = {
    val conditions = propertiesString.split(";").map(property => {
      if (isCensusCondition(property)) ""
      else if (isBoolean(property)) s"(${booleanCondition(property)})"
      else s"(${numericCondition(property)})"
    }).filter(_.length > 0)
    val censuses = propertiesString.split(";").map(property => {
      if (isCensusCondition(property)) s"${censusCondition(property)}"
      else ""
    }).filter(_.length > 0)
    val whereClause = StringBuilder.newBuilder
    if (propertiesString.nonEmpty) {
      whereClause.append(" WHERE ")
      if (conditions.nonEmpty) whereClause.append(conditions.drop(1).foldLeft(conditions.head)((a, b) => operatorJoin("AND", a, b)))
      if (conditions.nonEmpty && censuses.nonEmpty) whereClause.append(" AND ")
      if (censuses.nonEmpty) whereClause.append(s"(${censuses.drop(1).foldLeft(censuses.head)((a, b) => operatorJoin("OR", a, b))})")
    }
    whereClause.toString()
  }

  private def isCensusCondition(propertyString: String): Boolean = {
    propertyString.contains("symcubic_index") || propertyString.contains("cvt_index") || propertyString.contains("vt_index")
  }

  private def isBoolean(propertyString: String): Boolean = propertyString.contains("!") || !propertyString.contains(":")

  private def censusCondition(propertyString: String): String = {
    s"${quotedIdentifier(underscoreToCamel(propertyString))} IS NOT NULL"
  }

  private def booleanCondition(propertyString: String): String = {
    val name = if (propertyString.contains("!")) quotedIdentifier(underscoreToCamel(propertyString.drop(1))) else quotedIdentifier(underscoreToCamel(propertyString))
    else s"$name IS NOT NULL AND $name IS ${if (propertyString.contains("!")) "FALSE" else "TRUE"}"
  }

  private def numericCondition(propertyString: String): String = {
    val name = quotedIdentifier(underscoreToCamel(propertyString.split(":").head))
    val condition = propertyString.split(":").last.replace("*", "=")
    val operator = """^(=|==|<=|>=|<|>)(\d+\.?\d*)$""".r
    val interval = """^([\[\(])(\d+\.?\d*),?(\d+\.?\d*)([\]\)])$""".r
    condition match {
      case operator(op, num) => s"$name IS NOT NULL AND $name ${validOperatorString(op)} $num"
      case interval(leftBrace, lowerBound, upperBound, rightBrace) => s"$name IS NOT NULL AND ($lowerBound ${braceToOperator(leftBrace)} $name) AND ($name ${braceToOperator(rightBrace)} $upperBound)"
      case _ => sequenceToCondition(name, condition.split(","))
    }
  }

  private def validOperatorString(operatorString: String): String = {
    operatorString match {
      case "==" => "="
      case "!=" => "<>"
      case _ => operatorString
    }
  }

  private def quotedIdentifier(identifier: String): String = "\"" + identifier + "\""

  private def sequenceToCondition(name: String, seq: Seq[String]): String = {
    val strings = seq.map(n => s"($name = $n)")
    strings.drop(1).foldLeft(strings.head)((a, b) => operatorJoin("OR", a, b))
  }

  private def operatorJoin(op: String, a: Any, b: Any): String = s"$a $op $b"

  private def commaJoin(a: Any, b: Any): String = s"$a, $b"

  private def braceToOperator(brace: String): String = {
    if ("()".contains(brace)) "<"
    else "<="
  }

  private def camelToUnderscores(name: String) = "[A-Z\\d]".r.replaceAllIn(name, {m =>
    "_" + m.group(0).toLowerCase()
  })

  private def underscoreToCamel(name: String) = "_([a-z\\d])".r.replaceAllIn(name, {m =>
    m.group(1).toUpperCase()
  })

}
