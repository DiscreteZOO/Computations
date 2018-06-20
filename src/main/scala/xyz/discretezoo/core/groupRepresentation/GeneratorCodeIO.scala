package xyz.discretezoo.core.groupRepresentation

abstract class GeneratorCodeIO[T](protected val labels: Map[Generator, T],
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
