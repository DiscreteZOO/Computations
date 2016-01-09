import io.duality.TransactionManager.atomic

/**
  * Created by katja on 08/01/16.
  */
class SQLiteTable(db: SQLite, zooObjectStructure: ZooObjectStructure[_]) {

  private val rowIterator = db.getSQLiteIterator(zooObjectStructure.tableSQLite)

  def next = rowIterator.next()

  def get = {
    val obj = zooObjectStructure.constructFromSQLite(rowIterator)
    atomic {

    }
    obj
  }

}
