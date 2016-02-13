package xyz.discretezoo.core

import java.sql.{DriverManager, Statement}

import externalformats.SQLiteTable
import io.duality.PersistableSet
import io.duality.TransactionManager.atomic
import xyz.discretezoo.core.graphs.{ValidString6, Graph}

import scala.collection.mutable

/**
  * Created by katja on 02/01/16.
  */

class ZooCollection[T <: ZooObject](val zooObjectStructure: ZooObjectStructure[T]) {

  val persistableSet = new PersistableSet[T]

  def updateFromSQLite(): Unit = {

    val table = new SQLiteTable("discretezoo.db", zooObjectStructure)

    Range(0,212269).grouped(500).foreach(range => {
      println("next 500")
      atomic {
        range.foreach(range => {
          if (table.next) {
            val obj = table.get()
            this.persistableSet += obj
          }
        })
      }
    })

    table.close

    println("done")

//    compare properties, update if necessary

  }

  def insertAliases(): Unit = {
    val connection = DriverManager.getConnection(s"jdbc:sqlite:discretezoo.db")
    val statement: Statement = connection.createStatement()

    val aliases = new mutable.HashMap[String, mutable.Set[String]]() with mutable.MultiMap[String, String]
    val aliasIterator = statement.executeQuery(s"SELECT * FROM graph JOIN object_unique_id ON graph.zooid = object_unique_id.object_id JOIN object_alias ON graph.zooid = object_alias.object_id WHERE object_unique_id.algorithm = 'sage';")
    while (aliasIterator.next()) aliases.addBinding(aliasIterator.getString("unique_id"), aliasIterator.getString("alias"))

    val catalogIds = new mutable.HashMap[String, mutable.Set[CatalogId]]() with mutable.MultiMap[String, CatalogId]
    val vtIterator = statement.executeQuery(s"SELECT * FROM graph_vt JOIN graph USING (zooid) JOIN object_unique_id ON graph_vt.zooid = object_unique_id.object_id WHERE object_unique_id.algorithm = 'sage' AND graph.'order' <= 1280 AND graph_vt.vt_index IS NOT NULL;")
    while (vtIterator.next()) catalogIds.addBinding(vtIterator.getString("unique_id"), new CatalogId("vt", vtIterator.getInt("vt_index")))
    val symCubicIterator = statement.executeQuery(s"SELECT * FROM graph_cvt JOIN object_unique_id ON graph_cvt.zooid = object_unique_id.object_id WHERE object_unique_id.algorithm = 'sage' AND graph_cvt.symcubic_index IS NOT NULL;")
    while (symCubicIterator.next()) catalogIds.addBinding(symCubicIterator.getString("unique_id"), new CatalogId("cat", symCubicIterator.getInt("symcubic_index")))

    atomic {
      persistableSet.foreach(graph => {
        println(graph.uniqueId)
        aliases.get(graph.uniqueId).toSet.flatten.foreach(alias => { graph.aliases += alias })
        catalogIds.get(graph.uniqueId).toSet.flatten.foreach(cId => { graph.catalogues += cId })
      })
    }

  }

}
