package xyz.discretezoo.core.maniplexes.M2orbit

import xyz.discretezoo.core.groupRepresentation._
import xyz.discretezoo.core.primitives.LabelledEdge

class M2orbitManiplex(rank: Int, I: Set[Int]) extends AutomorphismGroup {

  override val N: Set[Int] = Range(0, rank).toSet
  private val J: Set[Int] = N.diff(I)

  override val groupName: String = s"G$rank"
  override val className: String = I.toSeq match {
    case Seq(i) => s"Class 2_$i"
    case s: Seq[Int] => s"Class 2_{${s.mkString(", ")}}"
  }

  /*
  * Private automorphism group generators
  * */

  // auxiliary generators
  private val A2ordered: Set[Generator] = J.subsets(2).map(pair => Generator(pair.toSeq.sorted)).toSet
  private val A2involutions: Set[Generator] = A2ordered.filter(g => g.subscript.last - g.subscript.head > 1)
  private val A2others: Set[Generator] = A2ordered.diff(A2involutions)

  // generators
  private val rho: Set[Generator] = I.map(i => Generator(i))
  private val alpha2: Set[Generator] = A2ordered ++ A2others.map(_.reverse) // the involutions only appear as j < k
  private val alpha3: Set[Generator] = I.flatMap(i => J.intersect(adjacent(i)).map(j => Generator(j, i, j)))

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

  /* OVERRIDES for AutomorphismGroup:
   * - getGenerators
   * - generatorNames
   * - generatorMAP
   * - relations
   * - Gamma
   */

  override protected def getGenerators: Seq[Generator] = Seq(rho, alpha2, alpha3).flatMap(_.toSeq)

  override val generatorNames: Map[Generator, String] = getGenerators.map(g => (g, g.subscript match {
    case Seq(i) => "r_" + i.toString
    case s: Seq[Int] => "a_{" + s.map(_.toString).mkString("") + "}"
    case _ => ""
  })).toMap

  // generator indices for code generation
  override protected val generatorMap: Map[Generator, Int] = {
    ((Generator(Seq()), 0) +: getGenerators.zipWithIndex.map(t => (t._1, t._2 + 1))).toMap
  }

  /* Relations for the finitely presented group
   * - relations from Hubard and Schulte
   * - additional relations where the generators not used as voltages are expressed in terms of the voltage generators
   */
  override protected def relations: Set[Relator] = {
    /*
    involutions */
    (rho ++ A2involutions ++ alpha3).map(g => Relator(g, 2)) ++
      /*
      alpha_{j, k} alpha_{k, j} = 1 */
      A2others.map(g => Relator(g, g.reverse)) ++
      /*
      alpha_{j, k} = alpha_{s, k} alpha_{j, s} */
      alpha2.flatMap(g => J.diff(g.subscript.toSet).map(s =>
        A2(s, g.subscript.last) + A2(g.subscript.head, s) ++ g.inverse
      )) ++
      /*
      alpha_{j, k}^{-1} rho_i alpha_{j, k} = alpha_{j, i, j}; |i-k| > 1, i in I */
      I.flatMap(i => J.diff(adjacent(i)).flatMap(k => (J - k).map(j =>
        R(i) + A2(j, k) + A3(j, i) ++ A2(j, k).inverse
      ))) ++
      /*
      alpha_{j, k} alpha_{s, t} alpha_{k, j} = alpha_{t, j} alpha_{j, s}; |j-k|, |s-k|, |t-k| > 1 */
      J.flatMap(k => subsequences(J - k, 3).map({
        case Seq(j, s, t) => A2(j, k) + A2(s, t) + A2(k, j) ++ (A2(t, j) + A2(j, s)).inverse
      })) ++
      /*
      alpha_{j, k} alpha_{s, i, s} alpha_{k, j} = alpha_{s, j} alpha_{k, i, k} alpha_{j, s}; i in I */
      J.flatMap(k => subsequences(J - k, 2).flatMap({
        case Seq(j, s) => I.toSeq.map(i => A2(j, k) + A3(s, i) + A2(k, j) ++ (A2(j, s) + A3(k, i) + A2(s, j)).inverse)
      })) ++
      /*
      alpha_{l, l-1, l} alpha_{l, l+1, l} = alpha_{l, l+1, l} alpha_{l, l-1, l}; l-1, l+1 in I */
      ((J.size, J.head) match {
        case (1, j) if Range(1, rank - 1).contains(j) =>
          Set(A3(j, j - 1) + A3(j, j + 1) ++ (A3(j, j + 1) + A3(j, j - 1)).inverse)
        case _ => Set()
      }) ++
      /*
      relations based on the symmetry graph with voltages
      the minimal element of J gets assigned the identity voltage (the corresponding edge is the chosen spanning tree) */
      {
        val min = J.min
        val nontrivialJ = J - min
        nontrivialJ.flatMap(k => {
          I.map(i => Relator(Generator(k, i, k)) ++ Relator(A2inverse(min, k), Generator(min, i, min), Generator(min, k))) ++
            nontrivialJ.filter(_ > k).map(l => Relator(A2inverse(k, l), Generator(min, l), A2inverse(min, k)))
        })
      }
  }

  /* Automorphism group
   * - intersection condition
   */
  override def Gamma(S: Set[Int]): Set[Generator] = {
    val intersectI = I.diff(S)
    val intersectJ = J.diff(S)
    intersectI.map(R) ++
      alpha3.filter(g => intersectJ.contains(g.subscript.head) && intersectI.contains(g.subscript.tail.head)) ++
      alpha2.filter(g => g.subscript.toSet.subsetOf(intersectJ))
  }



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



  /* Main function to obtain GAP code: automorphismGroupsGAP
   * - functions producing additional code needed
   */
  object codeSnippetsGAP extends GeneratorCodeIO(generatorMap, (i: Int) => s"G.$i") {

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
      val isRegular = s"""["regular", false <> GroupHomomorphismByImages(G, G, $generatorListNameGAP, $conjugatedGenerators)]"""
      s"[$isRegular]" // currently only one additional data
    }

    def automorphismGroupsGAP(order: Int): String = {
      //
      def g(g: Generator)  = s"$generatorListNameGAP[${generatorMap(g)}]"
      //
      def nonId: Seq[String] = generatorMap.values.filter(_ > 0).map(i => s"($generatorListNameGAP[$i] <> One(G))").toSeq
      // only rho_i and alpha_{j, i, j} for j minimal need to be checked
      def distinct: Seq[String] = {
        val alphas = alpha3.filter(_.subscript.head == J.min).subsets(2)
        (rho.subsets(2) ++ alphas).map(pair => s"(${g(pair.head)} <> ${g(pair.last)})").toSeq
      }
      GAP.setupForEpimorphismsToGroups(order, (nonId ++ distinct).mkString("(", " and ", ")"), additionalData)
    }

  }

}



/* Companion object
 */
object M2orbitManiplex {
  def deserialiseSymmetryType(st: String): Set[Int] = st.split("-").map(_.toInt).toSet
  def serialiseSymmetryType(I: Set[Int]): String = I.mkString("-")
}