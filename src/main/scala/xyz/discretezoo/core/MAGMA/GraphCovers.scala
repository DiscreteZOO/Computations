package xyz.discretezoo.core.MAGMA

import xyz.discretezoo.core.db.Maniplex
import xyz.discretezoo.core.groupRepresentation.Generator
import xyz.discretezoo.core.maniplexes.M2orbit.M2orbitManiplex
import xyz.discretezoo.core.primitives.{ActualPermutation, Identity, LabelledEdge, LabelledEdgeList}

case class GraphCovers(generators: List[List[Int]], symmetryType: String) {

  private val permutationGenerators = generators.map(ActualPermutation(_)).zipWithIndex.map(t => (t._2 + 1, t._1)).toMap
  def groupDegree: Int = permutationGenerators.values.map(_.max).max

  private val indexedEdgeVoltagePermutation: Map[Int, (LabelledEdge, Generator, ActualPermutation, Int)] = {
    def permutationFromData(generatorIndex: Int): ActualPermutation = {
      permutationGenerators.getOrElse(generatorIndex, Identity.ofDegree(groupDegree))
    }

    val I = M2orbitManiplex.deserialiseSymmetryType(symmetryType)
    val STGVoltages: Seq[(Int, (LabelledEdge, Generator))]= new M2orbitManiplex(3, I).symmetryTypeGraphWithVoltages
    val generatorPermutationMap: Map[Generator, ActualPermutation] = STGVoltages.map(t => {
      (t._2._2, permutationFromData(t._1)) // indexed by the generator maniplex index
    }).toMap
    val permutationIndexMap = generatorPermutationMap.filter(t => !t._2.isIdentity).zipWithIndex.map(t => {
      val permutationIndex = if (t._1._2.isIdentity) 0 else t._2 + 1
      (t._1._2, permutationIndex)
    }).toMap

    STGVoltages.map(t => {
      /* 1. 0 on the spanning tree and index of the voltage as generator of the maniplex elsewhere
       * 2. labelled edge, generator, corresponding permutation, and
       *    number of generator in the ordering of those used to generate the permutation group
      **/
      val permutation = generatorPermutationMap(t._2._2)
      val index = permutationIndexMap.getOrElse(permutation, 0)
      (t._1, (t._2._1, t._2._2, permutation, index))
    }).toMap
  }

  def permutationListString: String = {
    indexedEdgeVoltagePermutation.values.filter(_._4 > 0).map(t => (t._4, t._3)).map(t => {
      val permutationString = t._2.asCycles.map(c => {
        if (c.nonEmpty) c.map(_.toString).mkString("(", ",", ")")
      }).mkString
      (t._1, permutationString)
    }).toSeq.sortBy(_._1).map(_._2).mkString(", ")
  }
  def multiGraph: LabelledEdgeList = LabelledEdgeList(indexedEdgeVoltagePermutation.values.map(_._1).toSeq)
  def oddIndices: String = Range(1, 2 * multiGraph.list.size, 2).mkString("{@", ",", "@}")
  def voltages: String = indexedEdgeVoltagePermutation.map(t => s"G.${t._2._4}").mkString("[", ", ", "]")

  def code(outputFile: String): String = s"""
    |SetOutputFile("$outputFile");
    |load "GraphCovers.m";
    |G := PermutationGroup< $groupDegree| $permutationListString >;
    |${multiGraph.Magma}
    |Volt := $voltages;
    |oddIndices := $oddIndices;
    |VS := VoltageSpace("Permutation", G, X, oddIndices, semi, Volt);
    |M, newsemi, map := DerivedCover(VS);
    |if &or[newsemi(i): i in Indices(M)] then
    |  print "the multigraph has semiedges";
    |else
    |  E := EdgeSet(M);
    |  [<EndVertices(E.i), map(i)>: i in Indices(M)];
    |end if;
    |UnsetOutputFile();
    |exit;
    """.stripMargin

}

// &or[newsemi(i): i in Indices(M)];
// for e in EdgeSet(M) do e; end for;