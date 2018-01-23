package xyz.discretezoo.core.polytopes

import xyz.discretezoo.core.polytopes.groupRepresentation._

class P2orbit(rank: Int, I: Set[Int]) extends AutomorphismGroup {

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
  private val alpha2: Set[Generator] = A2ordered ++ A2others.map(_.reverse)
  private val alpha3: Set[Generator] = I.flatMap(i => J.intersect(adjacent(i)).map(j => Generator(j, i, j)))

  // shortcuts for generator constructors
  private def R(i: Int) = Generator(i)
  private def A2(i: Int, j: Int) = Generator(i, j)
  private def A3(i: Int, j: Int) = Generator(i, j, i)

  /*
  * AutomorphismGroup overrides
  * */

  override protected def getGenerators: Seq[Generator] = Seq(rho, alpha2, alpha3).flatMap(_.toSeq)

  override val generatorNames: Map[Generator, String] = getGenerators.map(g => (g, g.subscript match {
    case Seq(i) => "r_" + i.toString
    case s: Seq[Int] => "a_{" + s.map(_.toString).mkString("") + "}"
    case _ => ""
  })).toMap

  // generator indices for code generation
  override protected val generatorMap: Map[Generator, Int] = getGenerators.zipWithIndex.map(t => (t._1, t._2 + 1)).toMap

  //returns the set of all relations
  override protected def getRelations: Set[Relator] = {

    // i in I
    // j, k in J

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
      alpha_{j, k}^{-1} rho_i alpha_{j, k} = alpha_{j, i, j}; |i-k| > 1 */
      I.flatMap(i => J.diff(adjacent(i)).flatMap(k => (J - k).map(j =>
        R(i) + A2(j, k) + A3(j, i) ++ A2(j, k).inverse
      ))) ++
      /*
      alpha_{j, k} alpha_{s, t} alpha_{k, j} = alpha_{t, j} alpha_{j, s}; |j-k|, |s-k|, |t-k| > 1 */
      J.flatMap(k => subsequences(J - k, 3).map({
        case Seq(j, s, t) => A2(j, k) + A2(s, t) + A2(k, j) ++ (A2(t, j) + A2(j, s)).inverse
      })) ++
      /*
      alpha_{j, k} alpha_{s, i, s} alpha_{k, j} = alpha_{s, j} alpha_{k, i, k} alpha_{j, s} */
      J.flatMap(k => subsequences(J - k, 2).flatMap({
        case Seq(j, s) => I.toSeq.map(i => A2(j, k) + A3(s, i) + A2(k, j) ++ (A2(j, s) + A3(k, i) + A2(s, j)).inverse)
      })) ++
      /*
      alpha_{l, l-1, l} alpha_{l, l+1, l} = alpha_{l, l+1, l} alpha_{l, l-1, l} */ {
      (J.size, J.head) match {
        case (1, j) if Range(1, rank - 1).contains(j) =>
          Set(A3(j, j - 1) + A3(j, j + 1) ++ (A3(j, j + 1) + A3(j, j - 1)).inverse)
        case _ => Set()
      }
    }
  }

  override def Gamma(S: Set[Int]): Set[Generator] = {
    val intersectI = I.diff(S)
    val intersectJ = J.diff(S)
    intersectI.map(R) ++
      alpha3.filter(g => intersectJ.contains(g.subscript.head) && intersectI.contains(g.subscript.tail.head)) ++
      alpha2.filter(g => g.subscript.toSet.subsetOf(intersectJ))
  }

}
