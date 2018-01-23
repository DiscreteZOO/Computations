package xyz.discretezoo.core.polytopes.groupRepresentation

import xyz.discretezoo.core.externalprocess.Lowx

trait AutomorphismGroup {

  val N: Set[Int]
  val groupName: String
  val className: String

  protected val generatorMap: Map[Generator, Int]
  protected val generatorNames: Map[Generator, String]

  protected def getGenerators: Seq[Generator]
  protected def getRelations: Set[Relator]
  protected def Gamma(S: Set[Int]): Set[Generator]

  protected def adjacent(i: Int): Set[Int] = Set(i - 1, i, i + 1)
  protected def subsequences(J: Set[Int], size: Int): Iterator[Seq[Int]] = J.subsets(size).flatMap(_.toSeq.permutations)

  private def intToChar(i: Int): Char = (i - 1 + 'a'.toInt).toChar

  /*
  * Code generators
  * */

  abstract class CodeIO[T](protected val labels: Map[Generator, T],
                           stringify: T => String,
                           parenthesizeExponent: Boolean = false) {

    private def toCode(list: Seq[(Generator, Int)]) = {
      list.map(pair => (labels.get(pair._1), pair._2, parenthesizeExponent) match {
        case (Some(c), 1, _) => s"${stringify(c)}"
        case (Some(c), i, p) if !p || i < 10 => s"${stringify(c)}^$i"
        case (Some(c), i, true) => s"${stringify(c)}^{$i}"
        case _ => ""
      }).filter(_.nonEmpty).mkString("*")
    }

    protected def gpComponentCode(components: Seq[GroupComponent]): String = components.map(c => toCode(c match {
      case g: Generator => Seq((g, 1))
      case r: Relator => r.relation
    })).mkString(", ")

  }

  object GAP extends CodeIO(generatorMap, (i: Int) => s"$groupName.$i") {

    val code: String =
      s"""Free$groupName := FreeGroup("${generatorMap.values.map(i => s"g$i").mkString("\", \"")}");;"""

    def quotientCode(relators: Set[Relator]): String =
      s"$groupName := Free$groupName / [ ${gpComponentCode(getRelations.toSeq)}, ${gpComponentCode(relators.toSeq)} ];"

    private def getIntersectionConditions: Set[String] = {
      def subgroup(s: Set[Generator]) = s"Subgroup($groupName, [ ${gpComponentCode(s.toSeq)} ])"
      //TODO add conditions for |I| = n − 1
      N.subsets().toSet.subsets(2).map(x => (Gamma(x.head), Gamma(x.last), Gamma(x.head.union(x.last))))
      .filter(gammas => gammas._1.nonEmpty && gammas._2.nonEmpty).map(gammas => {
        val intersection = s"Intersection(${subgroup(gammas._1)}, ${subgroup(gammas._2)})"
        if (gammas._3.nonEmpty) s"$intersection = ${subgroup(gammas._3)};"
        else s"Size($intersection) = 1;"
      }).toSet
    }

  }

  object LOWX extends CodeIO(generatorMap.map(t => (t._1, intToChar(t._2))), (c: Char) => c.toString) {

    val charToGenerator: Map[Char, Generator] = generatorMap.map(t => (intToChar(t._2), t._1))
    private val generatorPattern = """([a-z])(\^-?[\d]+)?""".r

    def code: String = s"group = < ${labels.values.mkString(", ")} | ${gpComponentCode(getRelations.toSeq)} >"

    def parse(relation: String): Relator = Relator(relation.split('*').map({
      case c if c.length == 1 => (charToGenerator(c.head), 1)
      case generatorPattern(c, i) => (charToGenerator(c.head), i.tail.toInt)
    }).toSeq)

    def get(index: Int): Seq[(Int, Seq[Relator])] =
      new Lowx(code, index).runAndGetSubgroups.map(sd => (sd.index, sd.generators.map(parse))).toSeq

  }

  object LaTeX extends CodeIO(generatorNames, identity[String], parenthesizeExponent = true) {
    def code: String = s"$className: $$< ${getGenerators.mkString(", ")} | ${gpComponentCode(getRelations.toSeq)} >$$"
  }

}
