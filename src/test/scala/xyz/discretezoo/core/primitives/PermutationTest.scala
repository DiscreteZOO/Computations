package xyz.discretezoo.core.primitives

import org.scalatest.{FlatSpec, PrivateMethodTester}

class PermutationTest extends FlatSpec with PrivateMethodTester {

  val identity = ActualPermutation(Seq())

  behavior of "Permutation"

  // cyclesToMap

  it should "map identity as cycles to an empty map" in {
    assert(Map[Int, Int]() === ActualPermutation.cyclesToMap(Seq[Seq[Int]]()))
  }

  it should "map one cycle element to the next cycle element" in {
    assert(Map(1 -> 2, 2 -> 1) === ActualPermutation.cyclesToMap(Seq(Seq(1, 2))))
    assert(Map(1 -> 2, 2 -> 3, 3 -> 1) === ActualPermutation.cyclesToMap(Seq(Seq(1, 2, 3))))
    assert(Map(1 -> 2, 2 -> 1, 3 -> 4, 4 -> 5, 5 -> 3) === ActualPermutation.cyclesToMap(Seq(Seq(1, 2), Seq(3, 4, 5))))
    assert(Map(1 -> 2, 2 -> 1, 4 -> 5, 5 -> 4) === ActualPermutation.cyclesToMap(Seq(Seq(1, 2), Seq(4, 5))))
  }

  // mapToOneLine

  it should "map an empty map to an empty sequence" in {
    assert(Seq[Int]() === ActualPermutation.mapToOneLine(Map[Int, Int]()))
  }

  it should "order map values according to their keys and fill in the sequence with identities" in {
    assert(Seq[Int](2, 1) === ActualPermutation.mapToOneLine(Map(1 -> 2, 2 -> 1)))
    assert(Seq[Int](2, 1, 3, 5, 4) === ActualPermutation.mapToOneLine(Map(1 -> 2, 2 -> 1, 4 -> 5, 5 -> 4)))
  }

  // fromCycleNotation

  it should "map identity as cycles to an identity permutation" in {
    assert(identity === ActualPermutation.fromCycleNotation(Seq[Seq[Int]]()))
  }

  it should "order cycle elements according to their preceding elements and fill in the sequence with identities" in {
    assert(ActualPermutation(Seq(2, 1)) === ActualPermutation.fromCycleNotation(Seq(Seq(1, 2))))
    assert(ActualPermutation(Seq(2, 3, 1)) === ActualPermutation.fromCycleNotation(Seq(Seq(1, 2, 3))))
    assert(ActualPermutation(Seq(2, 1, 3, 5, 6, 4)) === ActualPermutation.fromCycleNotation(Seq(Seq(1, 2), Seq(4, 5, 6))))
  }

  // asCycles

  it should "only have cycles of length at least 2" in {
    assert(ActualPermutation(Seq(1, 2, 3)).asCycles === Seq())
    assert(ActualPermutation(Seq(2, 3, 1)).asCycles === Seq(Seq(1, 2, 3)))
    assert(ActualPermutation(Seq(2, 1, 3, 5, 6, 4)).asCycles === Seq(Seq(1, 2), Seq(4, 5, 6)))
  }

}
