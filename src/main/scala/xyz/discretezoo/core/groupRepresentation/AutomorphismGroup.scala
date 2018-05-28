package xyz.discretezoo.core.groupRepresentation

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

  /* GAP object
   */
  object GAP extends CodeIO(generatorMap, (i: Int) => s"$groupName.$i") {

    val code: String = s""""""

    val resources = "external/gap/"
    val newline = """\n"""

    private def intersectionConditions: Set[String] = {
      def subgroup(s: Set[Generator]) = s"Subgroup($groupName, [ ${gpComponentCode(s.toSeq)} ])"
      //TODO add conditions for |I| = n − 1
      N.subsets().toSet.subsets(2).map(x => (Gamma(x.head), Gamma(x.last), Gamma(x.head.union(x.last))))
      .filter(gammas => gammas._1.nonEmpty && gammas._2.nonEmpty).map(gammas => {
        val intersection = s"Intersection(${subgroup(gammas._1)}, ${subgroup(gammas._2)})"
        if (gammas._3.nonEmpty) s"$intersection = ${subgroup(gammas._3)};"
        else s"Size($intersection) = 1;"
      }).toSet
    }

    def setupForEpimorphismsToGpOrder(order: Int): String =
      s"""
         |Reread("${resources}improved_gquotient.g");
         |$groupName := FreeGroup("${generatorMap.values.map(i => s"g$i").mkString("\", \"")}");;
         |$groupName := $groupName / [ ${gpComponentCode(getRelations.toSeq)} ];;
         |for G in AllSmallGroups($order) do
         |    Print(IdSmallGroup(G)[2], "${newline}");
         |    smallGroup := Image(IsomorphismPermGroup(G));;
         |    Q := GQuotients(G3, smallGroup);;
         |    if Length(Q) > 0 then
         |        smaller := SmallerDegreePermutationRepresentation(smallGroup);;
         |        View(List(Q, hom -> List(GeneratorsOfGroup(G3), x -> Image(smaller, Image(hom, x)))));
         |        Print("${newline}");
         |    fi;
         |od;;
       """.stripMargin
  }

  /* LOWX object
   */
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

  /* LaTeX object
   */
  object LaTeX extends CodeIO(generatorNames, identity[String], parenthesizeExponent = true) {
    def code: String = s"$className: $$< ${getGenerators.mkString(", ")} | ${gpComponentCode(getRelations.toSeq)} >$$"
  }

}
