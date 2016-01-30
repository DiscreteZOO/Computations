package xyz.discretezoo.core.externalformats

import java.sql.{ResultSet, Statement, DriverManager}

/**
  * Created by katja on 25/01/16.
  */
class PostgresTable(jdbcConnectionString: String) {

  def filter(propertiesString: String): String = {

    val pgsql = DriverManager.getConnection(jdbcConnectionString)
    val query: Statement = pgsql.createStatement()

    val rowIterator = query.executeQuery(s"SELECT * FROM ${quotedIdentifier("xyz.discretezoo.core.graphs.Graph")}${constructWhereClause(propertiesString)};")

    val result = StringBuilder.newBuilder

    while (rowIterator.next()) result.append(s",${rowToJSON(rowIterator)}")

    query.close()
    pgsql.close()

    s"[${result.toString.drop(1)}]"

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
    val booleans = Seq("isBipartite", "isCayley", "isMoebiusLadder", "isPrism", "isSpx")
    val integers = Seq("order", "diameter", "girth", "oddGirth")
    val intermediate = booleans.map(property => s"${quotedIdentifier(camelToUnderscores(property))}: ${resultSet.getBoolean(property).toString}").foldLeft(s"${quotedIdentifier("id")}: ${quotedIdentifier(resultSet.getString("uniqueId"))}")(commaJoin)
    val jsonString = integers.map(property => s"${quotedIdentifier(camelToUnderscores(property))}: ${resultSet.getInt(property).toString}").foldLeft(intermediate)(commaJoin)
    s"{ $jsonString }"
  }

  private def constructWhereClause(propertiesString: String): String = {
    val conditions = propertiesString.split(";").map(property => {
      if (isBoolean(property)) s"(${booleanCondition(property)})"
      else s"(${numericCondition(property)})"
    })
    if (propertiesString.nonEmpty) s" WHERE ${conditions.drop(1).foldLeft(conditions.head)((a, b) => operatorJoin("AND", a, b))}"
    else ""
  }

  private def isBoolean(propertyString: String): Boolean = propertyString.contains("!") || !propertyString.contains(":")

  private def booleanCondition(propertyString: String): String = {
    if (propertyString.contains("!")) s"${quotedIdentifier(underscoreToCamel(propertyString.drop(1)))} IS FALSE"
    else s"${quotedIdentifier(underscoreToCamel(propertyString))} IS TRUE"
  }

  private def numericCondition(propertyString: String): String = {
    val name = quotedIdentifier(underscoreToCamel(propertyString.split(":").head))
    val condition = propertyString.split(":").last.replace("*", "=")
    val operator = """^(=|==|<=|>=|<|>)(\d+\.?\d*)$""".r
    val interval = """^([\[\(])(\d+\.?\d*),?(\d+\.?\d*)([\]\)])$""".r
    condition match {
      case operator(op, num) => s"$name ${validOperatorString(op)} $num"
      case interval(leftBrace, lowerBound, upperBound, rightBrace) => s"($lowerBound ${braceToOperator(leftBrace)} $name) AND ($name ${braceToOperator(rightBrace)} $upperBound)"
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
