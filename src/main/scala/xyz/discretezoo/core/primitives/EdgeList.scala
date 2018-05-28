package xyz.discretezoo.core.primitives

// (vertex1, vertex2, edgeLabel)
case class EdgeList(list: Seq[LabelledEdge]) {

  private def vertices: Seq[Int] = list.toSet.flatMap((edge: LabelledEdge) => Set(edge._1, edge._2)).toSeq

  // assume it is reduced
  def Magma: String = {
    s"""X := MultiGraph< ${vertices.size} | ${list.map(_.Magma).mkString(", ")} >;
      |semi := [${list.map(_.isSemiedge)}];""".stripMargin
  }

}