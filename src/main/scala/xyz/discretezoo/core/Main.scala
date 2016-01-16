package xyz.discretezoo.core

import io.duality.{PersistableSet, Database}
import io.duality.TransactionManager.atomic
import xyz.discretezoo.core.graphs.{LabelledVertexNeighbourhood, Graph}

/**
 * Created by katja on 20/08/15.
 */

object Main {

  val graphs = new ZooCollection(Graph)

  def main (args: Array[String]) {

    val db = new Database("jdbc:pgsql://localhost:5432/graphzoo?user=graphzoo&password=gr4ph!Z00")

    db.connectRoot(this)
    graphs.updateFromSQLite

//    Graph.loadFromInit()

    atomic {
//      println(Graph.dynamicProperties)
      // transaction
      // podatke samo enkrat vlece iz baze, sicer bi jih vsakic znova
      // knjiznica naceloma atomic bloke kreira sama
      // rabimo kjer delamo z vsemi elementi
    }

  }

}