package xyz.discretezoo.core.groupRepresentation

import xyz.discretezoo.core.externalprocess.Lowx
import xyz.discretezoo.core.util.DZConfig

trait AutomorphismGroup {

  val N: Set[Int]
  val groupName: String
  val className: String

  protected val generatorMap: Map[Generator, Int]
  protected val generatorNames: Map[Generator, String]

  protected def getGenerators: Seq[Generator]
  protected def relations: Set[Relator]
  protected def Gamma(S: Set[Int]): Set[Generator]

  protected def adjacent(i: Int): Set[Int] = Set(i - 1, i, i + 1)
  protected def subsequences(J: Set[Int], size: Int): Iterator[Seq[Int]] = J.subsets(size).flatMap(_.toSeq.permutations)

  private def intToChar(i: Int): Char = (i - 1 + 'a'.toInt).toChar

  /* GAP object
   */
  object GAP extends GeneratorCodeIO(generatorMap, (i: Int) => s"$groupName.$i") {

    /* GapBatch adds an outputFile variable prepared for output
    * */

    val code: String = s""""""
    val newline = """\n"""

    def intersectionConditions: Set[String] = {
      def subgroup(s: Set[Generator]) = s"Subgroup($groupName, [ ${gpComponentCode(s.toSeq)} ])"
      //TODO add conditions for |I| = n − 1
      N.subsets().toSet.subsets(2).map(x => (Gamma(x.head), Gamma(x.last), Gamma(x.head.union(x.last))))
      .filter(gammas => gammas._1.nonEmpty && gammas._2.nonEmpty).map(gammas => {
        val intersection = s"Intersection(${subgroup(gammas._1)}, ${subgroup(gammas._2)})"
        if (gammas._3.nonEmpty) s"$intersection = ${subgroup(gammas._3)};"
        else s"Size($intersection) = 1;"
      }).toSet
    }

    def setupForEpimorphismsToGroups(order: Int, homomorphismCondition: String, additionalDataCode: String): String = {
      val maybeComma = if (additionalDataCode.nonEmpty) ", " else ""
      val generatorList = generatorMap.values.toSeq.filter(_ > 0).sorted.map(i => s"g$i").mkString("\", \"")
      //
      //
      // H := Image(smallerPG);;
      s"""
         |Reread("${DZConfig.externalResourcesGAP}improved_gquotient.g");
         |
         |PGGeneratorList := function(FGG, G, outputFile) # FGG finitely generated group, G SmallGroup
         |    local Q, smallPG, smallerPG, gen, q;
         |    Q := GQuotients(FGG, G);;
         |    if Length(Q) > 0 then
         |        smallPG := IsomorphismPermGroup(G);;
         |        smallerPG := SmallerDegreePermutationRepresentation(Image(smallPG));;
         |        for q in Q do
         |            gen := List(GeneratorsOfGroup(FGG), x -> Image(q, x));;
         |            if $homomorphismCondition then
         |                IO_Write(outputFile, [List(gen, x -> Image(smallerPG, Image(smallPG, x))), $additionalDataCode]);;
         |            fi;
         |        od;;
         |        IO_WriteLine(outputFile, "");
         |        GASMAN("collect");
         |    fi;
         |end;
         |
         |$groupName := FreeGroup("$generatorList");;
         |$groupName := $groupName / [ ${gpComponentCode(relations.toSeq)} ];;
         |for G in AllSmallGroups($order) do
         |    IO_Write(outputFile, IdSmallGroup(G)[2]);
         |    IO_WriteLine(outputFile, "");
         |    PGGeneratorList(G3, G, outputFile);
         |od;;
       """.stripMargin
      // g:=(1,2)*(3,5,4);
      // List([1..6], x->x^g);
    }

  }

  /* LOWX object
   */
  object LOWX extends GeneratorCodeIO(generatorMap.map(t => (t._1, intToChar(t._2))), (c: Char) => c.toString) {

    val charToGenerator: Map[Char, Generator] = generatorMap.map(t => (intToChar(t._2), t._1))
    private val generatorPattern = """([a-z])(\^-?[\d]+)?""".r

    def code: String = s"group = < ${labels.values.mkString(", ")} | ${gpComponentCode(relations.toSeq)} >"

    def parse(relation: String): Relator = Relator(relation.split('*').map({
      case c if c.length == 1 => (charToGenerator(c.head), 1)
      case generatorPattern(c, i) => (charToGenerator(c.head), i.tail.toInt)
    }).toSeq)

    def get(index: Int): Seq[(Int, Seq[Relator])] =
      new Lowx(code, index).runAndGetSubgroups.map(sd => (sd.index, sd.generators.map(parse))).toSeq

  }

  /* LaTeX object
   */
  object LaTeX extends GeneratorCodeIO(generatorNames, identity[String], parenthesizeExponent = true) {
    def code: String = s"$className: $$< ${getGenerators.mkString(", ")} | ${gpComponentCode(relations.toSeq)} >$$"
  }

}
