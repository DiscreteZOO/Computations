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

    val table = new SQLiteTable("graphzoo.db", zooObjectStructure)

    atomic {
      var counter = 0
      while (table.next && counter < 3000) {
        val obj = table.get()
        if (this.persistableSet.exists(o => o.uniqueId == obj.uniqueId)) println("found one")
        else this.persistableSet += obj
        counter += 1
      }
    }

    table.close

//    compare properties, update if necessary

  }

}
