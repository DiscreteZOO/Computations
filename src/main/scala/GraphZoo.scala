import io.duality.{PersistableSet, Database}

/**
 * Created by katja on 20/08/15.
 */

object GraphZoo {

  val graphs = new PersistableSet[Graph] // anything inherited from Graph?

  def main (args: Array[String]) {
    val db = new Database("jdbc:pgsql://localhost:5432/graphzoo?user=graphzoo&password=gr4ph!Z00")
    db.connectRoot(this)

    val sqliteDB = new SQLite("graphzoo.db")
    sqliteDB.getAllGraphs.foreach(m => {
      if (m.get("size").get == 60) {
        val g = new Graph(new String6(m.get("data").get.toString).parse)
        println(g.description)
        DynamicProperties.booleanProperties.foreach(p => {

        })
      }
    })

    val adjacenciesString = "[[0,1], [1, 2], [2,0]]"
    val graph = new RegularGraph(Util.buildAdjacencyList(adjacenciesString))
//    println(graph)



    val multigraph = GraphFamilies.doubleCycle(3)
//    println(multigraph)

    val px = GraphFamilies.praegerXu(3, 1)
//    println(px)

    val v1 = new Vertex
    val v2 = new Vertex
    val v3 = new Vertex
    val v4 = new Vertex
    val e1 = new Edge(v1, v2)
    val e2 = new Edge(v2, v3)
    val e3 = new Edge(v3, v4)
//    println(ArcN.vertexSequence(Seq(e2, e1, e3)))

  }

}