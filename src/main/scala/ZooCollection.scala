import io.duality.TransactionManager.atomic

/**
  * Created by katja on 02/01/16.
  */

class ZooCollection(val zooObjectStructure: ZooObjectStructure[_]) {

  val persistableSet = zooObjectStructure.persistableSet

  def updateFromSQLite(db: SQLite): Unit = {

    val table = new SQLiteTable(db, zooObjectStructure)

    table.next
    println(table.get)

    atomic {
//      while (table.next) {
//
//      }
    }

//    check if graph exists in database via canonical labelling
//    if not, insert
//    else compare properties, update if necessary

  }

}
