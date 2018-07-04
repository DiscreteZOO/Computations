package xyz.discretezoo.core.maniplexes.M2orbit

import org.scalatest.{FlatSpec, PrivateMethodTester}
import xyz.discretezoo.core.groupRepresentation.{Generator, Relator}

class M2orbitManiplexTest extends FlatSpec with PrivateMethodTester {

  private val r0 = Generator(Seq(0))
  private val r1 = Generator(Seq(1))
  private val r2 = Generator(Seq(2))
  private val a02 = Generator(Seq(0, 2))
  private val a01 = Generator(Seq(0, 1))
  private val a10 = Generator(Seq(1, 0))
  private val a12 = Generator(Seq(1, 2))
  private val a21 = Generator(Seq(2, 1))
  private val a010 = Generator(Seq(0,1,0))
  private val a101 = Generator(Seq(1, 0, 1))
  private val a121 = Generator(Seq(1, 2, 1))
  private val a212 = Generator(Seq(2, 1, 2))

  def inv(g: Generator): Relator = Relator(Seq((g, 2)))

  behavior of "M2orbitManiplex"
  /* - should compute generator relations according to Hubard and Schulte
   * - should compute the voltage map so that (where j = J.min)
   *   * semiedges of the first flag orbit get voltages rho_i with labels i
   *   * semiedges of the second flag orbit get voltages alpha_{k, i, k}, k in {j-1, j+1} and rho_i otherwise
   *   * the rest of the edges except for the one labelled j get voltage alpha_{j, k}, k in J TODO inverse?
  **/

  it should "correctly compute relations and voltages for rank 3, class {}" in {
    val M = new M2orbitManiplex(3, Set())
    assert(M.relations == Set(Relator(a02, 2), Relator(a01, a10), Relator(a12, a21),
      Relator(a10, a21, a02), Relator(a02, a12, a01), Relator(a21, a02, a10)))
  }

  it should "correctly compute relations and voltages for rank 3, class {0}" in {
    val M = new M2orbitManiplex(3, Set(0))
    assert(M.relations == Set(inv(r0), inv(a101), Relator(a12, a21), Relator(a101) ++ a12.inverse ++ Relator(r0, a12)))
  }

  it should "correctly compute relations and voltages for rank 3, class {1}" in {
    val M = new M2orbitManiplex(3, Set(1))
    assert(M.relations == Set(inv(r1), inv(a02), inv(a010), inv(a212), Relator(a212) ++ Relator(a02, a010, a02)))
    assert(M.voltagesMap == (Map(1 -> 1), Map(2 -> 2), Map(3 -> 1)))
  }

  it should "correctly compute relations and voltages for rank 3, class {2}" in {
    val M = new M2orbitManiplex(3, Set(2))
    assert(M.relations == Set(inv(r2), inv(a121), Relator(a01, a10), Relator(a121) ++ a10.inverse ++ Relator(r2, a10)))
  }

  it should "correctly compute relations and voltages for rank 3, class {0, 1}" in {
    val M = new M2orbitManiplex(3, Set(0, 1))
    assert(M.relations == Set(inv(r0), inv(r1), inv(a212)))
  }

  it should "correctly compute relations and voltages for rank 3, class {0, 2}" in {
    val M = new M2orbitManiplex(3, Set(0, 2))
    assert(M.relations == Set(inv(r0), inv(r2), inv(a101), inv(a121), Relator(a101, a121) ++ Relator(a121, a101).inverse))
  }

  it should "correctly compute relations and voltages for rank 3, class {1, 2}" in {
    val M = new M2orbitManiplex(3, Set(1, 2))
    assert(M.relations == Set(inv(r1), inv(r2), inv(a010)))
  }

}
