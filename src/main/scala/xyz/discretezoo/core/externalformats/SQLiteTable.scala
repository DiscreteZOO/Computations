package xyz.discretezoo.core.externalformats

import java.sql.{DriverManager, Statement}

import io.duality.TransactionManager.atomic
import xyz.discretezoo.core.{Property, PropertyValue, ZooObjectStructure}
import xyz.discretezoo.core.graphs.Graph

/**
  * Created by katja on 08/01/16.
  */
class SQLiteTable(dbName: String, zooObjectStructure: ZooObjectStructure[_]) {

  private val connection = DriverManager.getConnection(s"jdbc:sqlite:$dbName")
  private val statement: Statement = connection.createStatement()

  statement.setQueryTimeout(30)

  private val results = statement.executeQuery(s"PRAGMA table_info('${zooObjectStructure.tableSQLite}');") // cid, name, type, notnull, dflt_value, pk

  def getUniqueId(zooId: Int): String = {
//    val uniqueIdQuery = connection.createStatement()
//    try {
//      uniqueIdQuery.setQueryTimeout(30)
//      val result = uniqueIdQuery.executeQuery(s"SELECT unique_id FROM 'object' WHERE zooid = '$zooId';")
//      println(result.getString("unique_id"))
//      result.getString("unique_id")
//
//    }
//    catch {
//      case e: Exception => println("Some exception");
//    } finally {
//      uniqueIdQuery.close
//      ""
//    }
    ""
  }

//  second part

  private val rowIterator = statement.executeQuery(s"SELECT * FROM ${zooObjectStructure.tableSQLite} JOIN object ON ${zooObjectStructure.tableSQLite}.zooid = object.zooid;")

  def next = rowIterator.next()

  def get = {
    val obj = Graph.constructFromSQLite(rowIterator)

    atomic {
      zooObjectStructure.properties.foreach(property => {
        if (rowIterator.getObject(property.name) != null) property.propertyType.name match {
          case "Boolean" => {
            val result = rowIterator.getBoolean(property.name)
            if (result != null) obj.properties += new PropertyValue[Boolean](property.asInstanceOf[Property[Boolean]], result)
          }
          case "Double" => {
            val result = rowIterator.getDouble(property.name)
            if (result != null) obj.properties += new PropertyValue[Double](property.asInstanceOf[Property[Double]], result)
          }
          case "Integer" => {
            val result = rowIterator.getInt(property.name)
            if (result != null) obj.properties += new PropertyValue[Int](property.asInstanceOf[Property[Int]], result)
          }
          case "String" => {
            val result = rowIterator.getString(property.name)
            if (result != null) obj.properties += new PropertyValue[String](property.asInstanceOf[Property[String]], result)
          }
          case "Rational" => {}
        }
      })

    }
    obj
  }

  def close = {
    statement.close()
    connection.close()
  }

}
