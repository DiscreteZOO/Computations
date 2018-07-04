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
    ((Generator(Seq()), 0) +: getGenerators.zipWithIndex.map(t => (t._1, t._2 + 1))).toMap
  }

  override def relations: Set[Relator] = Generators.relations

  // intersection condition
  override def Gamma(S: Set[Int]): Set[Generator] = Generators.Gamma(S)

  def symmetryTypeGraphWithVoltages: Seq[(Int, (LabelledEdge, Generator))] = SymmetryTypeGraph.symmetryTypeGraphWithVoltages

  def voltagesMap: (Map[Int, Int], Map[Int, Int], Map[Int, Int]) = SymmetryTypeGraph.voltagesMap


  /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
   * Main function to obtain GAP code: automorphismGroupsGAP
   * - functions producing additional code needed
   */
  object GAPCodeSnippets extends GeneratorCodeIO(generatorMap, (i: Int) => s"gen[$i]") {

    private val generatorListNameGAP = "gen"
    // small group name: G

    private def conjugatedGenerators: String = {
      val j = J.min
      val neighbours = Seq(j - 1, j + 1)
      def isConjugationNonTrivial(subscript: Seq[Int]): Boolean = subscript.exists(i => neighbours.contains(i))
      val newGenerators = generatorMap.filter(_._2 > 0).toSeq.sortBy(_._2).map(_._1).map(g => {
        if (isConjugationNonTrivial(g.subscript)) {
          g.subscript match {
            case Seq(i) => Relator(Generator(j, i, j))
            case Seq(k, l) => Generator(j, l).inverse ++ Relator(Generator(j, k))
            case Seq(k, i, _) =>  Generator(j, k).inverse ++ Relator(Generator(i), Generator(j, k))
          }
        }
        else Relator(g)
      })
      s"[${gpComponentCode(newGenerators)}]"
    }

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

    val rho: Set[Generator] = I.map(i => Generator(i))
    val alpha2: Set[Generator] = A2ordered ++ A2others.map(_.reverse) // the involutions only appear as j < k
    val alpha3: Set[Generator] = I.flatMap(i => J.intersect(adjacent(i)).map(j => Generator(j, i, j)))

    // utility functions
    private def adjacent(i: Int): Set[Int] = Set(i - 1, i, i + 1)

    // auxiliary generators
    private def A2ordered: Set[Generator] = J.subsets(2).map(pair => Generator(pair.toSeq.sorted)).toSet
    private def A2involutions: Set[Generator] = A2ordered.filter(g => g.subscript.last - g.subscript.head > 1)
    private def A2others: Set[Generator] = A2ordered.diff(A2involutions)

    // shortcuts for generator constructors
    private def R(i: Int) = Generator(i)
    private def A2(i: Int, j: Int) = Generator(i, j)
    private def A3(i: Int, j: Int) = Generator(i, j, i)
    private def A2inverse(i: Int, j: Int) = {
      val indices = Set(i, j)
      val min = indices.min
      val max = indices.max
      if (max - min == 1) Generator(j, i)
      else Generator(min, max)
    }

    def all: Seq[Generator] = Seq(rho, alpha2, alpha3).flatMap(_.toSeq)

    /* Relations for the finitely presented group
     * - relations from Hubard and Schulte
     * - additional relations where the generators not used as voltages are expressed in terms of the voltage generators
     */

    def involutionRelations: Set[Relator] = (rho ++ A2involutions ++ alpha3).map(g => Relator(g, 2))

    // alpha_{j, k} alpha_{k, j} = 1
    def A2inverseRelations: Set[Relator] = A2others.map(g => Relator(g, g.reverse))

    // alpha_{j, k} = alpha_{s, k} alpha_{j, s}
    def A2shortConsecutiveRelations: Set[Relator] = {
      J.flatMap(j => J.filter(_ > j).flatMap(k => J.diff(Set(j, k)).map(s =>
        Relator(A2inverse(j, k), A2(s, k), A2(j, s))
      )))
    }

    // alpha_{j, k}^{-1} rho_i alpha_{j, k} = alpha_{j, i, j}; |i-k| > 1, i in I
    def A2conjugationRelations: Set[Relator] = {
      I.flatMap(i => J.intersect(adjacent(i)).flatMap(j => J.diff(adjacent(i)).map(k =>
        Relator(A3(j, i)) ++ A2(j, k).inverse ++ Relator(R(i), A2(j, k))
      )))
    }

    // alpha_{j, k} alpha_{s, t} alpha_{k, j} = alpha_{t, j} alpha_{j, s}; |j-k|, |s-k|, |t-k| > 1
    def A2longConsecutiveRelations: Set[Relator] = J.flatMap(k => subsequences(J - k, 3).map({
      case Seq(j, s, t) => A2(j, k) + A2(s, t) + A2(k, j) ++ (A2(t, j) + A2(j, s)).inverse
    }))

    // alpha_{j, k} alpha_{s, i, s} alpha_{k, j} = alpha_{s, j} alpha_{k, i, k} alpha_{j, s}; i in I
    def A3relations: Set[Relator] = I.flatMap(i => {
      def getRelators(k: Int, s: Int): Set[Relator] = {
        def equationSide(j1: Int, j2: Int, j3: Int): Relator = A2(j1, j2) + A3(j3, i) + A2inverse(j1, j2)
        J.diff(adjacent(k) + s).map(j => equationSide(j, k, s) ++ equationSide(s, j, k))
      }
      val adj = J.intersect(adjacent(i))
      if (adj.size == 2) getRelators(adj.head, adj.last) ++ getRelators(adj.last, adj.head)
      else Seq()
    })

    // alpha_{l, l-1, l} alpha_{l, l+1, l} = alpha_{l, l+1, l} alpha_{l, l-1, l}; l-1, l+1 in I, |I| = n-1
    def A3adjacentIndicesRelations: Set[Relator] = {
      (J.size, J.head) match {
        case (1, j) if Range(1, rank - 1).contains(j) => // l = 0, n: not ok
          Set(A3(j, j - 1) + A3(j, j + 1) ++ (A3(j, j + 1) + A3(j, j - 1)).inverse)
        case _ => Set()
      }
    }

    /* relations based on the symmetry graph with voltages
     * the minimal element of J gets assigned the identity voltage;
     * its corresponding edge is the chosen spanning tree
     **/
    def relationsSTG: Set[Relator] = {
      val min = J.min
      val nontrivialJ = J - min
      val A2relations = nontrivialJ.flatMap(k => {
        nontrivialJ.filter(_ > k).map(l => Relator(A2inverse(k, l), A2(min, l), A2inverse(min, k)))
      })
      val A3relations = I.intersect(adjacent(min)).flatMap(i => nontrivialJ.intersect(adjacent(i)).map(k =>
        Relator(Generator(k, i, k)) ++ Relator(A2(min, k), A3(min, i), A2inverse(min, k))
      ))
      A2relations ++ A3relations
    }

    def relations: Set[Relator] = {
      involutionRelations ++ A2inverseRelations ++
        A2shortConsecutiveRelations ++ A2longConsecutiveRelations ++ A2conjugationRelations ++
        A3relations ++ A3adjacentIndicesRelations ++ relationsSTG
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
      (rho.subsets(2) ++ alpha3.filter(_.subscript.head == J.min).subsets(2)).map(_.toSeq)
        .map(pair => (pair.head, pair.last)).toSeq
    }

  }

  private object SymmetryTypeGraph {

    /* - all edges need to be from the smaller vertex to the larger vertex
     * - the index of the spanning tree identity is 0 (only one such)
     */
    def symmetryTypeGraphWithVoltages: Seq[(Int, (LabelledEdge, Generator))] = {

      val j = J.min

      def semiedges: Seq[(LabelledEdge, Generator)] = {
        I.toSeq.sorted.flatMap(i => Seq((LabelledEdge(1, 1, i), Generator(i)), (LabelledEdge(2, 2, i), Generator(j, i, j))))
      }
      def undirectedEdges: Seq[(LabelledEdge, Generator)] = {
        J.filter(_ - j > 1).toSeq.sorted.map(k => (LabelledEdge(1, 2, k), Generator(j, k)))
      }
      // voltage on the directed edge (2, 1) is alpha_{l, l+1}, hence the inverse on (1, 2)
      def directedEdges: Seq[(LabelledEdge, Generator)] = {
        J.filter(_ - j == 1).toSeq.sorted.map(k => (LabelledEdge(1, 2, k), Generator(k, j)))
      }

      // generator index -> (edge, generator)
      def edgesWithVoltages: Seq[(LabelledEdge, Generator)] = semiedges ++ undirectedEdges ++ directedEdges
      def identity: (LabelledEdge, Generator) = (LabelledEdge(1, 2, j), Generator(Seq()))
      def allEdges: Seq[(LabelledEdge, Generator)] = identity +: edgesWithVoltages
      allEdges.map(t => (generatorMap(t._2), (t._1, t._2)))
    }

    /* Returns voltages of the symmetry type graph
     * - a triple of maps by voltage generator type (subscript length)
     * - each map of all voltage generators of that type, index -> label (0, 1, ...)
     * TODO add rho_i's for alpha_{j, i, j} where i and j commute
     */
    def voltagesMap: (Map[Int, Int], Map[Int, Int], Map[Int, Int]) = {

      def rhosSecondOrbit: Map[Int, Int] = I.diff(Set(J.min - 1, J.min + 1)).map(i => (generatorMap(Generator(i)), i)).toMap

      def voltages(length: Int) = {
        generatorMap.filter(p => {
          p._2 > 0 && p._1.subscript.length == length && ((length == 1) || p._1.subscript.head == J.min)
        }).map(pair => length match {
          case 1 => (pair._2, pair._1.subscript.head)
          case 2 => (pair._2, pair._1.subscript.last)
          case 3 => (pair._2, pair._1.subscript.tail.head)
        })
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