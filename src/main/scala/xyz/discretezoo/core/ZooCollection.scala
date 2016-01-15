package xyz.discretezoo.core

import xyz.discretezoo.core.externalformats.SQLiteTable

/**
  * Created by katja on 02/01/16.
  */

class ZooCollection(val zooObjectStructure: ZooObjectStructure[_]) {

  val persistableSet = zooObjectStructure.persistableSet

  def updateFromSQLite: Unit = {

    val table = new SQLiteTable("graphzoo.db", zooObjectStructure)

//    while (table.next) {
      table.next
      val obj = table.get
//    }

    table.close
//    check if graph exists in database via canonical labelling
//    if not, insert
//    else compare properties, update if necessary

  }

}
