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
  private def R(i: Int) = Relator(Generator(i))
  private def A2(j: Int, k: Int) = {
    val s = Set(j, k)
    if (s.max - s.min > 1) Relator(Generator(s.min, s.max), 1)
    else Relator(Generator(Seq(j, k).sorted), if (j < k) 1 else -1)
  }
  private def A3(j: Int, i: Int) = Relator(Generator(j, i, j))

  // utility functions
  private def adjacent(i: Int): Set[Int] = Set(i - 1, i, i + 1)
  private def isNextToMinJ(i: Int): Boolean = Seq(minJ - 1, minJ + 1).contains(i)
  private def nontrivialGeneratorMap: Map[Generator, Int] = generatorMap.filter(_._2 > 0)



  /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
   * OVERRIDES for AutomorphismGroup:
   * - getGenerators
   * - generatorNames
   * - generatorMAP
   * - relations
   * - intersectionConditions
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
  override def intersectionConditions: Map[(Set[Generator], Set[Generator]), Set[Generator]] = Generators.intersectionConditions

  /* Symmetry type graph methods
   * (exposed from the SymmetryTypeGraph private object)
   */
  def symmetryTypeGraphWithVoltages: Seq[(Int, (LabelledEdge, Generator))] = SymmetryTypeGraph.symmetryTypeGraphWithVoltages
  def voltagesMap: (Map[Int, Int], Map[Int, Int], Map[Int, Int]) = SymmetryTypeGraph.voltagesMap



  /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
   * Main function to obtain GAP code: automorphismGroupsGAP
   * - functions producing additional code needed
   * - small group name: G
   */

  object GAPCodeSnippets extends GeneratorCodeIO(generatorMap, (i: Int) => s"gen[$i]") {

    private val generatorListNameGAP = "gen"

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



  /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
   * Generator computation
   * - all: returns the list of all nontrivial generators,
   * - relations: returns the list of all group relations,
   * - distinctPairs:
   * - intersectionConditions:
   */

  private object Generators {

    private def rho: Set[Generator] = I.map(i => Generator(i))
    private def alpha2: Set[Generator] = A2ordered // elements alpha_{j, k} only appear as j < k
    private def alpha3: Set[Generator] = I.flatMap(i => J.intersect(adjacent(i)).map(j => Generator(j, i, j)))

    // auxiliary generator lists
    private def A2ordered: Set[Generator] = orderedPairsJ.map(pair => Generator(pair)).toSet
    private def A2involutions: Set[Generator] = A2ordered.filter(g => g.subscript.last - g.subscript.head > 1)

    // utility functions
    private def alphaOrRho(j: Int, i: Int): Generator = if (adjacent(j).contains(i)) Generator(j, i, j) else Generator(i)
    private def alphaOrRhoVoltage(i: Int): Generator = if (isNextToMinJ(i)) Generator(minJ, i, minJ) else Generator(i)
    private def orderedPairsJ: Seq[Seq[Int]] = orderedPairs(J)
    private def orderedPairs(s: Set[Int]): Seq[Seq[Int]] = s.subsets(2).map(_.toSeq.sorted).toSeq

    /* Relations for the finitely presented group
     * - relations from Hubard and Schulte, referenced in []
     * - always invert the right-hand side (where one of the sides is not identity)
     * - additional relations where the generators not used as voltages are expressed in terms of the voltage generators
     * - alpha_{j, i, j} = rho_i if |j-i| > 1: extra conditions needed in (6), but not (4), (7)
     * - inverse relations for elements alpha_{j, k} which are not involutions are not needed:
     *   (2) [2.1.3] alpha_{j, k} alpha_{k, j} = 1
     */

    // (1) [2.1.1, 2.1.2] rho_i^2 = alpha_{j, k}^2 = alpha_{j, i, j}^2 = 2; |j-k| > 1
    private def involutionRelations: Set[Relator] = (rho ++ A2involutions ++ alpha3).map(g => Relator(g, 2))

    // (3) [2.2] alpha_{s, k} alpha_{j, s} = alpha_{j, k}
    private def A2shortConsecutiveRelations: Set[Relator] = {
      orderedPairsJ.flatMap(p => p match { // for each pair j < k one relation
        case Seq(j, k) => J.diff(p.toSet).map(s => A2(s, k) ++ A2(j, s) ++ A2(k, j))
      }).toSet
    }

    // (4) [3.1] alpha_{j, k}^{-1} rho_i alpha_{j, k} = alpha_{j, i, j}; |i-k| > 1, i in I; trivial if |i-j| > 1
    private def A2conjugationRelations: Set[Relator] = {
      I.flatMap(i => J.intersect(adjacent(i)).flatMap(j => J.diff(adjacent(i)).map(k =>
        A2(k, j) ++ R(i) ++ A2(j, k) ++ A3(j, i)
      )))
    }

    // (5) [3.2] alpha_{j, k} alpha_{s, t} alpha_{k, j} = alpha_{t, j} alpha_{j, s}; |j-k|, |s-k|, |t-k| > 1
    private def A2longConsecutiveRelations: Set[Relator] = {
      val nonDegenerateConditions: Set[Relator] = J.flatMap(k => { // only j < k, s < t
        J.filter(_ < k - 1).flatMap(j => orderedPairs(J.diff(adjacent(k) + j)).map({
          case Seq(s, t) => A2(j, k) ++ A2(s, t) ++ A2(k, j) ++ (A2(t, j) ++ A2(j, s)).inverse
        }))
      })
      val degenerateConditions: Set[Relator] = J.flatMap(k => { // only j < k, t < j
        orderedPairs(J.filter(_ < k - 1)).map({ case Seq(t, j) => A2(j, k) ++ A2(j, t) ++ A2(k, j) ++ A2(j, t)})
      })
      nonDegenerateConditions ++ degenerateConditions
    }

    // (6) [3.3] alpha_{j, k} alpha_{s, i, s} alpha_{k, j} = alpha_{s, j} alpha_{k, i, k} alpha_{j, s}; i in I, |j-k|, |s-k| > 1
    private def A3relations: Set[Relator] = I.flatMap(i => {
      // j and s distinct, only j < k (alpha_{j, k} involution)
      def equationSide(j1: Int, j2: Int, j3: Int): Relator = A2(j1, j2) ++ Relator(alphaOrRho(j3, i)) ++ A2(j2, j1)
      val nonDegenerateConditions = J.flatMap(k => J.filter(_ < k - 1).flatMap(j => {
        J.diff(adjacent(k) + j).map(s => equationSide(j, k, s) ++ equationSide(s, j, k).inverse)
      }))
      // j = s; j, k are i +/- 1
      val degenerateConditions = {
        val candidates = J.intersect(adjacent(i))
        if (candidates.size == 2) Set(A2(i - 1, i + 1) ++ A3(i - 1, i) ++ A2(i + 1, i - 1) ++ A3(i + 1, i))
        else Set[Relator]()
      }
      nonDegenerateConditions ++ degenerateConditions
    })

    // (7) [4] alpha_{l, l-1, l} alpha_{l, l+1, l} = alpha_{l, l+1, l} alpha_{l, l-1, l}; l-1, l+1 in I, |I| = n-1
    private def A3adjacentIndicesRelations: Set[Relator] = {
      (J.size, J.head) match {
        case (1, j) if Range(1, rank - 1).contains(j) => // l = 0, n: not ok
          Set(A3(j, j - 1) ++ A3(j, j + 1) ++ (A3(j, j + 1) ++ A3(j, j - 1)).inverse)
        case _ => Set()
      }
    }

    // subset of the generator corresponding to the subset S of N, used for the intersection condition
    private def Gamma(S: Set[Int]): Set[Generator] = {
      val intersectI = I.diff(S)
      val intersectJ = J.diff(S)
      intersectI.map(Generator(_)) ++
        alpha3.filter(g => intersectJ.contains(g.subscript.head) && intersectI.contains(g.subscript.tail.head)) ++
        alpha2.filter(g => g.subscript.toSet.subsetOf(intersectJ))
    }

    /* PUBLIC METHODS */

    def all: Seq[Generator] = Seq(rho, alpha2, alpha3).flatMap(_.toSeq)

    def relations: Set[Relator] = {
      involutionRelations ++ // (1)
        A2shortConsecutiveRelations ++ // (3)
        A2conjugationRelations ++ // (4)
        A2longConsecutiveRelations ++ // (5)
        A3relations ++ // (6)
        A3adjacentIndicesRelations // (7)
    }

    /* A sequence of pairs of generators that need to be checked for distinctness
     * - only rho_i and alpha_{j, i, j} for j minimal need to be distinct
     * (from the voltages of the symmetry type graph)
     */
    def distinctPairs: Seq[(Generator, Generator)] = {
      I.subsets(2).flatMap(pair => {
        val i1 = pair.head
        val i2 = pair.last
        Set((Generator(i1), Generator(i2)), (alphaOrRhoVoltage(i1), alphaOrRhoVoltage(i2)))
      }).toSeq
    }

    /* Produce a new list of generators, such that each element is "conjugated" by the minimal element of J
     * - used to test regularities
     */
    def conjugatedGenerators: Seq[Relator] = {
      // if all the indices in the subscript commute with minJ, we get the same generator
      def isConjugationNonTrivial(subscript: Seq[Int]): Boolean = subscript.exists(i => isNextToMinJ(i))
      // sequence of nontrivial generators ordered by index, conjugated
      nontrivialGeneratorMap.toSeq.sortBy(_._2).map(_._1).map(g => {
        if (isConjugationNonTrivial(g.subscript)) {
          g.subscript match {
            case Seq(i) => A3(minJ, i)
            case Seq(k, l) => A2(l, minJ) ++ A2(minJ, k)
            case Seq(k, i, _) =>  A2(k, minJ) ++ R(i) ++ A2(minJ, k)
          }
        }
        else Relator(g)
      })
    }

    def intersectionConditions: Map[(Set[Generator], Set[Generator]), Set[Generator]] = {

      // extra condition if |I| = n-1; j0 splits I into two ranges; i is j0 +/- 1
      def condition(s: Set[Int]): Map[(Set[Generator], Set[Generator]), Set[Generator]] = {
        def partialCondition(i: Int) =
          Map((Set(Generator(i), Generator(minJ, i, minJ)), s.map(alphaOrRhoVoltage)) -> Set(Generator(minJ, i, minJ)))
        if (s.contains(0)) partialCondition(s.max)
        else if (s.contains(rank - 1)) partialCondition(s.min)
        else Map()
      }
      val extraCondition = {
        if (J.size == 1) condition(I.filter(_ < J.head)) ++ condition(I.filter(_ > J.head))
        else Map()
      }

      N.flatMap(s => N.subsets(s)).subsets(2).map(x => {
        (Gamma(x.head), Gamma(x.last)) -> Gamma(x.head.union(x.last))
      }).filter(gammas => gammas._1._1.nonEmpty && gammas._1._2.nonEmpty).toMap ++ extraCondition
    }

  }

  private object SymmetryTypeGraph {

    /* - all edges need to be from the smaller vertex to the larger vertex
     * - the index of the spanning tree identity is 0 (only one such)
     * TODO alpha_{j, k}: they need to be inverted if k = j+1
     */
    def symmetryTypeGraphWithVoltages: Seq[(Int, (LabelledEdge, Generator))] = {

      def semiedges: Seq[(LabelledEdge, Generator)] = {
        I.toSeq.sorted.flatMap(i => Seq((LabelledEdge(1, 1, i), Generator(i)), (LabelledEdge(2, 2, i), Generator(minJ, i, minJ))))
      }
      // voltage on the directed edge (2, 1) is alpha_{l, l+1}, hence the inverse on (1, 2)
      def edges: Seq[(LabelledEdge, Generator)] = {
        J.toSeq.sorted.map(k => (LabelledEdge(1, 2, k), Generator(minJ, k)))
      }

      // generator index -> (edge, generator)
      def edgesWithVoltages: Seq[(LabelledEdge, Generator)] = semiedges ++ edges
      def identity: (LabelledEdge, Generator) = (LabelledEdge(1, 2, minJ), Id)
      def allEdges: Seq[(LabelledEdge, Generator)] = identity +: edgesWithVoltages
      allEdges.map(t => (generatorMap(t._2), (t._1, t._2)))
    }

    /* Returns voltages of the symmetry type graph
     * - a triple of maps by voltage generator type (subscript length)
     * - each map of all voltage generators of that type, index -> label (0, 1, ...)
     * Map 1: all generators rho_i (labels i)
     * Map 2: all generators alpha_{j, k}, j = min{J} (labels k)
     * Map 3: all generators alpha{j, i, j}, j = min{J}, i = j +/- 1 and rho_i, |i-j| > 1 (labels i)
     */
    def voltagesMap: (Map[Int, Int], Map[Int, Int], Map[Int, Int]) = {

      // if |i-min{J}| > 1, the voltage on the corresponding semiedge is rho_i
      def rhosSecondOrbit: Map[Int, Int] = I.diff(adjacent(minJ)).map(i => (generatorMap(Generator(i)), i)).toMap

      def voltages(length: Int): Map[Int, Int] = {
        def edgeLabel(generator: Generator): Int = generator.subscript match {
          case Seq(i) => i
          case Seq(j, k) => k
          case Seq(j, i, _) => i
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
  def deserialiseSymmetryType(st: String): Set[Int] = {
    if (st.isEmpty) Set()
    else st.split("-").map(_.toInt).toSet
  }
  def serialiseSymmetryType(I: Set[Int]): String = I.mkString("-")
}