package xyz.discretezoo.core

import externalformats.SQLiteTable
import io.duality.PersistableSet
import io.duality.TransactionManager.atomic
import xyz.discretezoo.core.graphs.{ValidString6, Graph}

/**
  * Created by katja on 02/01/16.
  */

class ZooCollection[T <: ZooObject](val zooObjectStructure: ZooObjectStructure[T]) {

  val persistableSet = new PersistableSet[T]

  def updateFromSQLite(): Unit = {

    val table = new SQLiteTable("discretezoo.db", zooObjectStructure)

    Range(0,111360).grouped(500).foreach(range => {
      println("next 500")
      atomic {
        range.foreach(range => {
          if (table.next) {
            val obj = table.get()
            if (!this.persistableSet.exists(o => o.uniqueId == obj.uniqueId)) this.persistableSet += obj
          }
        })
      }
    })

    table.close

    println("done")

//    compare properties, update if necessary

  }

}
