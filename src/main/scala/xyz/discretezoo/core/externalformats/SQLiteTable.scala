package xyz.discretezoo.core.externalformats

import java.sql.{DriverManager, Statement}

import io.duality.TransactionManager.atomic
import xyz.discretezoo.core.{ZooObject, Property, PropertyValue, ZooObjectStructure}

/**
  * Created by katja on 08/01/16.
  */
class SQLiteTable[T <: ZooObject](dbName: String, zooObjectStructure: ZooObjectStructure[T]) {

  private val connection = DriverManager.getConnection(s"jdbc:sqlite:$dbName")
  private val statement: Statement = connection.createStatement()

  statement.setQueryTimeout(30)

  private val results = statement.executeQuery(s"PRAGMA table_info('${zooObjectStructure.tableSQLite}');") // cid, name, type, notnull, dflt_value, pk

  private val rowIterator = statement.executeQuery(s"SELECT * FROM ${zooObjectStructure.tableSQLite} JOIN object ON ${zooObjectStructure.tableSQLite}.zooid = object.zooid;")

  def next = rowIterator.next()

  def get(): T = {

    val obj = zooObjectStructure.constructFromSQLite(rowIterator)

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
