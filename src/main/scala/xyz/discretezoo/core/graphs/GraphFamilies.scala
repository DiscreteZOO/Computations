package xyz.discretezoo.core.graphs

/**
  * Created by katja on 23/11/15.
  */
object GraphFamilies {

  def doubleCycle(length: Int): Multigraph = {
    new Multigraph((0 until length).map(v => ((v, (v+1) % length), 2)).toMap[Tuple2[Int, Int], Int])
  }

  def graphArcN(multigraph: Multigraph, n: Int) = {

    val vertexIndex = multigraph.verticesByIds.map(_.swap)

    def isStandardArc(arc: ArcN): Boolean = vertexIndex(arc.vertices.head) < vertexIndex(arc.vertices.last)

    val arcsByIds = multigraph.edges.toSeq.combinations(n).flatMap(_.permutations).filter(ArcN.isArc).map(new ArcN(_)).filter(isStandardArc).zipWithIndex.map(_.swap).toMap
    println(arcsByIds.size)
//
//    def adjacentArcs(arc: ArcN): Set[ArcN] = {
//      val a = arcsByIds.filter(pair => pair._2.isShunt(arc) || pair._2.isShunt(arc.reverse)).values.toSet
//      a
//    }
//
//    val arcsToIds = arcsByIds.map(_.swap)
//    val adjacencies = arcsByIds.map(pair => (pair._1, adjacentArcs(pair._2).map(arcsToIds(_))))
//    new Graph(Util.buildAdjacencyList(adjacencies))

  }

  def praegerXu(n: Int, s: Int) = { // : RegularGraph

    require(n > 0 && s > 0, "Both parameters should be positive.")
    require(n > s, "Parameter n should be larger than s.") // TODO

    graphArcN(GraphFamilies.doubleCycle(n), s)

  }

}
