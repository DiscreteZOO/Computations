package xyz.discretezoo.core.maniplexes.M2orbit

import org.scalatest.{FlatSpec, PrivateMethodTester}
import xyz.discretezoo.core.groupRepresentation.{Generator, Relator}

class M2orbitManiplexTest extends FlatSpec with PrivateMethodTester {

  private val r0 = Generator(0)
  private val r1 = Generator(1)
  private val r2 = Generator(2)
  private val a01 = Generator(0, 1)
  private val a02 = Generator(0, 2)
  private val a12 = Generator(1, 2)
  private val a010 = Generator(0, 1, 0)
  private val a101 = Generator(1, 0, 1)
  private val a121 = Generator(1, 2, 1)
  private val a212 = Generator(2, 1, 2)

  def r(g: Generator): Relator = Relator(g)
  def i(g: Generator): Relator = Relator(g, -1)
  def sq(g: Generator): Relator = Relator(g, 2)

  behavior of "M2orbitManiplex"
  /* - should compute generator relations according to Hubard and Schulte
   * - should compute the voltage map so that (where j = J.min)
   *   * semiedges of the first flag orbit get voltages rho_i with labels i
   *   * semiedges of the second flag orbit get voltages alpha_{k, i, k}, k in {j-1, j+1} and rho_i otherwise
   *   * the rest of the edges except for the one labelled j get voltage alpha_{j, k}, k in J TODO inverse?
  **/

  it should "correctly compute properties for class {}" in {
    val M3 = new M2orbitManiplex(3, Set())
    assert(M3.relations == Set(sq(a02), i(a12) ++ r(a02) ++ i(a01), a12 + a01 + a02, r(a02) ++ i(a01) ++ i(a12)))
    val M4 = new M2orbitManiplex(4, Set())
  }

  it should "correctly compute properties for class {0}" in {
    val M3 = new M2orbitManiplex(3, Set(0))
    assert(M3.relations == Set(sq(r0), sq(a101), i(a12) + r0 + a12 + a101))
    val M4 = new M2orbitManiplex(4, Set(0))
  }

  it should "correctly compute properties for class {1}" in {
    val M3 = new M2orbitManiplex(3, Set(1))
    assert(M3.relations == Set(sq(r1), sq(a02), sq(a010), sq(a212), a02 + a010 + a02 + a212))
    val M4 = new M2orbitManiplex(4, Set(1))
  }

  it should "correctly compute properties for class {2}" in {
    val M3 = new M2orbitManiplex(3, Set(2))
    assert(M3.relations == Set(sq(r2), sq(a121), r(a01) ++ r(r2) ++ i(a01) + a121))
    val M4 = new M2orbitManiplex(4, Set(2))
  }

  it should "correctly compute properties for class {0, 1}" in {
    val M3 = new M2orbitManiplex(3, Set(0, 1))
    assert(M3.relations == Set(sq(r0), sq(r1), sq(a212)))
    val M4 = new M2orbitManiplex(4, Set(0, 1))
  }

  it should "correctly compute properties for class {0, 2}" in {
    val M3 = new M2orbitManiplex(3, Set(0, 2))
    assert(M3.relations == Set(sq(r0), sq(r2), sq(a101), sq(a121), a101 + a121 ++ (a121 + a101).inverse))
    val M4 = new M2orbitManiplex(4, Set(0, 2))
  }

  it should "correctly compute properties for class {1, 2}" in {
    val M3 = new M2orbitManiplex(3, Set(1, 2))
    assert(M3.relations == Set(sq(r1), sq(r2), sq(a010)))

    val a03 = Generator(0, 3)
    val a323 = Generator(3, 2, 3)
    val M4 = new M2orbitManiplex(4, Set(1, 2))
    assert(M4.relations == Set(sq(r1), sq(r2), sq(a03), sq(a010), sq(a323), a03 + r1 + a03 + a010, a03 + r2 + a03 + a323))
  }

}
