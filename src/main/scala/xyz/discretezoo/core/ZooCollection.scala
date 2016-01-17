package xyz.discretezoo.core

import externalformats.SQLiteTable
import io.duality.PersistableSet
import xyz.discretezoo.core.graphs.{ValidString6, Graph}

/**
  * Created by katja on 02/01/16.
  */

class ZooCollection[T <: ZooObject](val zooObjectStructure: ZooObjectStructure[T]) {

  val persistableSet = new PersistableSet[T]

  def updateFromSQLite(): Unit = {

    val table = new SQLiteTable("graphzoo.db", zooObjectStructure)

    table.next
//    while (table.next) {
    val obj = table.get()
//    this.persistableSet += obj
//    }

    table.close
//    check if graph exists in database via canonical labelling
//    if not, insert
//    else compare properties, update if necessary

  }

}
