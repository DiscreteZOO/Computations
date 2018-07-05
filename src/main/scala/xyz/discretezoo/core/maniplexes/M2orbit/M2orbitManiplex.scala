package xyz.discretezoo.core.maniplexes.M2orbit

import shapeless.ops.tuple.Length
import xyz.discretezoo.core.groupRepresentation._
import xyz.discretezoo.core.primitives.LabelledEdge

case class M2orbitManiplex(rank: Int, I: Set[Int]) extends AutomorphismGroup {

  override val N: Set[Int] = Range(0, rank).toSet

  protected val J: Set[Int] = N.diff(I)
  protected val minJ: Int = J.min

  override val groupName: String = s"G$rank"
  override val className: String = I.toSeq match {
    case Seq(i) => s"Class 2_$i"
    case s: Seq[Int] => s"Class 2_{${s.mkString(", ")}}"
  }

  // shortcuts for generator constructors
  private val Id = Generator(Seq())
  private def R(i: Int) = Generator(i)
  private def A2(j: Int, k: Int) = Generator(j, k)
  private def A3(j: Int, i: Int) = Generator(j, i, j)
  private def A2inverse(j: Int, k: Int) = {
    val indices = Set(j, k)
    val min = indices.min
    val max = indices.max
    if (max - min == 1) Generator(k, j)
    else Generator(min, max)
  }



  /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
   * OVERRIDES for AutomorphismGroup:
   * - getGenerators
   * - generatorNames
   * - generatorMAP
   * - relations
   * - Gamma
   */

  override def getGenerators: Seq[Generator] = Generators.all

  override val generatorNames: Map[Generator, String] = getGenerators.map(g => (g, g.subscript match {
    case Seq(i) => "r_" + i.toString
    case s: Seq[Int] => "a_{" + s.map(_.toString).mkString("") + "}"
    case _ => ""
  })).toMap

  // generator indices for code generation
  override val generatorMap: Map[Generator, Int] = {
    ((Id, 0) +: getGenerators.zipWithIndex.map(t => (t._1, t._2 + 1))).toMap
  }

  override def relations: Set[Relator] = Generators.relations

  // intersection condition
  override def Gamma(S: Set[Int]): Set[Generator] = Generators.Gamma(S)

  // symmetry type graph
  def symmetryTypeGraphWithVoltages: Seq[(Int, (LabelledEdge, Generator))] = SymmetryTypeGraph.symmetryTypeGraphWithVoltages
  def voltagesMap: (Map[Int, Int], Map[Int, Int], Map[Int, Int]) = SymmetryTypeGraph.voltagesMap

  private def nontrivialGeneratorMap: Map[Generator, Int] = generatorMap.filter(_._2 > 0)



  /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
   * Main function to obtain GAP code: automorphismGroupsGAP
   * - functions producing additional code needed
   */

  object GAPCodeSnippets extends GeneratorCodeIO(generatorMap, (i: Int) => s"gen[$i]") {

    private val generatorListNameGAP = "gen"
    // small group name: G

    private def conjugatedGenerators: String = s"[${gpComponentCode(Generators.conjugatedGenerators)}]"

    def additionalData: String = {
      val isRegular = s"""["regular", fail <> GroupHomomorphismByImages(G, G, $generatorListNameGAP, $conjugatedGenerators)]"""
      s"[$isRegular]" // currently only one additional data
    }

    def automorphismGroupsGAP(order: Int): String = {
      def g(g: Generator)  = s"$generatorListNameGAP[${generatorMap(g)}]"
      def nonId: Seq[String] = generatorMap.values.filter(_ > 0).map(i => s"($generatorListNameGAP[$i] <> One(G))").toSeq
      def distinct: Seq[String] = Generators.distinctPairs.map(pair => s"(${g(pair._1)} <> ${g(pair._2)})")
      GAP.setupForEpimorphismsToGroups(order, (nonId ++ distinct).mkString("(", " and ", ")"), additionalData)
    }

  }

  private object Generators {

    val rho: Set[Generator] = I.map(i => R(i))
    val alpha2: Set[Generator] = A2ordered ++ A2others.map(_.reverse) // the involutions only appear as j < k
    val alpha3: Set[Generator] = I.flatMap(i => J.intersect(adjacent(i)).map(j => A3(j, i)))

    // utility functions
    private def adjacent(i: Int): Set[Int] = Set(i - 1, i, i + 1)

    // auxiliary generators
    private def A2ordered: Set[Generator] = J.subsets(2).map(pair => Generator(pair.toSeq.sorted)).toSet
    private def A2involutions: Set[Generator] = A2ordered.filter(g => g.subscript.last - g.subscript.head > 1)
    private def A2others: Set[Generator] = A2ordered.diff(A2involutions)

    def all: Seq[Generator] = Seq(rho, alpha2, alpha3).flatMap(_.toSeq)

    /* Relations for the finitely presented group
     * - relations from Hubard and Schulte
     * - additional relations where the generators not used as voltages are expressed in terms of the voltage generators
     */

    // (1) rho_i^2 = alpha_{j, k}^2 = alpha_{j, i, j}^2 = 2; |j-k| > 1
    def involutionRelations: Set[Relator] = (rho ++ A2involutions ++ alpha3).map(g => Relator(g, 2))

    // (2) alpha_{j, k} alpha_{k, j} = 1
    def A2inverseRelations: Set[Relator] = A2others.map(g => Relator(g, g.reverse))

    // (3) alpha_{j, k} = alpha_{s, k} alpha_{j, s}
    def A2shortConsecutiveRelations: Set[Relator] = {
      J.flatMap(j => J.filter(_ > j).flatMap(k => J.diff(Set(j, k)).map(s =>
        Relator(A2inverse(j, k), A2(s, k), A2(j, s))
      )))
    }

    // (4) alpha_{j, k}^{-1} rho_i alpha_{j, k} = alpha_{j, i, j}; |i-k| > 1, i in I
    def A2conjugationRelations: Set[Relator] = {
      I.flatMap(i => J.intersect(adjacent(i)).flatMap(j => J.diff(adjacent(i)).map(k =>
        Relator(A3(j, i)) ++ A2(j, k).inverse ++ Relator(R(i), A2(j, k))
      )))
    }

    // (5) alpha_{j, k} alpha_{s, t} alpha_{k, j} = alpha_{t, j} alpha_{j, s}; |j-k|, |s-k|, |t-k| > 1
    def A2longConsecutiveRelations: Set[Relator] = J.flatMap(k => subsequences(J - k, 3).map({
      case Seq(j, s, t) => A2(j, k) + A2(s, t) + A2(k, j) ++ (A2(t, j) + A2(j, s)).inverse
    }))

    // (6) alpha_{j, k} alpha_{s, i, s} alpha_{k, j} = alpha_{s, j} alpha_{k, i, k} alpha_{j, s}; i in I
    def A3relations: Set[Relator] = I.flatMap(i => {
      def getRelators(k: Int, s: Int): Set[Relator] = {
        def equationSide(j1: Int, j2: Int, j3: Int): Relator = A2(j1, j2) + A3(j3, i) + A2inverse(j1, j2)
        J.diff(adjacent(k) + s).map(j => equationSide(j, k, s) ++ equationSide(s, j, k))
      }
      val adj = J.intersect(adjacent(i))
      if (adj.size == 2) getRelators(adj.head, adj.last) ++ getRelators(adj.last, adj.head)
      else Seq()
    })

    // (7) alpha_{l, l-1, l} alpha_{l, l+1, l} = alpha_{l, l+1, l} alpha_{l, l-1, l}; l-1, l+1 in I, |I| = n-1
    def A3adjacentIndicesRelations: Set[Relator] = {
      (J.size, J.head) match {
        case (1, j) if Range(1, rank - 1).contains(j) => // l = 0, n: not ok
          Set(A3(j, j - 1) + A3(j, j + 1) ++ (A3(j, j + 1) + A3(j, j - 1)).inverse)
        case _ => Set()
      }
    }

    /* (8) relations based on the symmetry graph with voltages
     * the minimal element of J gets assigned the identity voltage;
     * its corresponding edge is the chosen spanning tree
     */
    def relationsSTG: Set[Relator] = {
      val nontrivialJ = J - minJ
      val A2relations = nontrivialJ.flatMap(k => {
        nontrivialJ.filter(_ > k).map(l => Relator(A2inverse(k, l), A2(minJ, l), A2inverse(minJ, k)))
      })
      val A3relations = I.intersect(adjacent(minJ)).flatMap(i => nontrivialJ.intersect(adjacent(i)).map(k =>
        Relator(A3(k, i)) ++ Relator(A2(minJ, k), A3(minJ, i), A2inverse(minJ, k))
      ))
      A2relations ++ A3relations
    }

    def relations: Set[Relator] = {
      involutionRelations ++ A2inverseRelations ++ // (1), (2)
        A2shortConsecutiveRelations ++ A2conjugationRelations ++ A2longConsecutiveRelations ++ // (3), (4), (5)
        A3relations ++ A3adjacentIndicesRelations ++ relationsSTG // (6), (7), (8)
    }

    /* Automorphism group
     * - intersection condition
     */
    def Gamma(S: Set[Int]): Set[Generator] = {
      val intersectI = I.diff(S)
      val intersectJ = J.diff(S)
      intersectI.map(R) ++
        alpha3.filter(g => intersectJ.contains(g.subscript.head) && intersectI.contains(g.subscript.tail.head)) ++
        alpha2.filter(g => g.subscript.toSet.subsetOf(intersectJ))
    }

    /* Gives a sequence of pairs of generators that need to be checked for distinctness
     * - only rho_i and alpha_{j, i, j} for j minimal need to be distinct
     * (from the voltages of the symmetry type graph)
     */
    def distinctPairs: Seq[(Generator, Generator)] = {
      (rho.subsets(2) ++ alpha3.filter(_.subscript.head == minJ).subsets(2)).map(_.toSeq)
        .map(pair => (pair.head, pair.last)).toSeq
    }

    /* Produce a new list of generators, such that each element is "conjugated" by the minimal element of J
     * - used to test regularities
     */
    def conjugatedGenerators: Seq[Relator] = {
      // if all the indices in the subscript commute with minJ, we get the same generator
      def isConjugationNonTrivial(subscript: Seq[Int]): Boolean = subscript.exists(i => Seq(minJ - 1, minJ + 1).contains(i))
      // sequence of nontrivial generators ordered by index, conjugated
      nontrivialGeneratorMap.toSeq.sortBy(_._2).map(_._1).map(g => {
        if (isConjugationNonTrivial(g.subscript)) {
          g.subscript match {
            case Seq(i) => Relator(A3(minJ, i))
            case Seq(k, l) => A2(minJ, l).inverse ++ Relator(A2(minJ, k))
            case Seq(k, i, _) =>  A2(minJ, k).inverse ++ Relator(R(i), A2(minJ, k))
          }
        }
        else Relator(g)
      })
    }

  }

  private object SymmetryTypeGraph {

    /* - all edges need to be from the smaller vertex to the larger vertex
     * - the index of the spanning tree identity is 0 (only one such)
     */
    def symmetryTypeGraphWithVoltages: Seq[(Int, (LabelledEdge, Generator))] = {

      def semiedges: Seq[(LabelledEdge, Generator)] = {
        I.toSeq.sorted.flatMap(i => Seq((LabelledEdge(1, 1, i), R(i)), (LabelledEdge(2, 2, i), A3(minJ, i))))
      }
      def undirectedEdges: Seq[(LabelledEdge, Generator)] = {
        J.filter(_ - minJ > 1).toSeq.sorted.map(k => (LabelledEdge(1, 2, k), A2(minJ, k)))
      }
      // voltage on the directed edge (2, 1) is alpha_{l, l+1}, hence the inverse on (1, 2)
      def directedEdges: Seq[(LabelledEdge, Generator)] = {
        J.filter(_ - minJ == 1).toSeq.sorted.map(k => (LabelledEdge(1, 2, k), A2(k, minJ)))
      }

      // generator index -> (edge, generator)
      def edgesWithVoltages: Seq[(LabelledEdge, Generator)] = semiedges ++ undirectedEdges ++ directedEdges
      def identity: (LabelledEdge, Generator) = (LabelledEdge(1, 2, minJ), Id)
      def allEdges: Seq[(LabelledEdge, Generator)] = identity +: edgesWithVoltages
      allEdges.map(t => (generatorMap(t._2), (t._1, t._2)))
    }

    /* Returns voltages of the symmetry type graph
     * - a triple of maps by voltage generator type (subscript length)
     * - each map of all voltage generators of that type, index -> label (0, 1, ...)
     * TODO add rho_i's for alpha_{j, i, j} where i and j commute
     */
    def voltagesMap: (Map[Int, Int], Map[Int, Int], Map[Int, Int]) = {

      // if |i-min{J}| > 1, the voltage on the corresponding semiedge is rho_i
      def rhosSecondOrbit: Map[Int, Int] = I.diff(Set(minJ - 1, minJ + 1)).map(i => (generatorMap(R(i)), i)).toMap

      def voltages(length: Int): Map[Int, Int] = {
        def edgeLabel(generator: Generator): Int = length match {
          case 1 => generator.subscript.head
          case 2 => generator.subscript.last
          case 3 => generator.subscript.tail.head
        }
        nontrivialGeneratorMap.filter(p => { // filter
          p._1.subscript.length == length && ((length == 1) || p._1.subscript.head == minJ)
        }).map(p => (p._2, edgeLabel(p._1)))
      }

      (voltages(1), voltages(2), voltages(3) ++ rhosSecondOrbit)
    }

  }

}



/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Companion object
 */
object M2orbitManiplex {
  def deserialiseSymmetryType(st: String): Set[Int] = st.split("-").map(_.toInt).toSet
  def serialiseSymmetryType(I: Set[Int]): String = I.mkString("-")
}