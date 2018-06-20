package xyz.discretezoo.core.GAP

import xyz.discretezoo.core.primitives.Permutation

case class HomomorphismWithProperties(homomorphism: Seq[Permutation], properties: Seq[HomomorphismProperty]) {

  val satisfiedAdditionalRelations: Seq[Boolean] = {
    val rels = properties.filter(p => p.name == "relations").map(_.value)
    if (rels.nonEmpty) rels.head.split(',').map(stringToBoolean)
    else Seq()
  }

  // assumes we are generating 2-orbit maniplexes
  val orbits: Int = {
    val reg = properties.filter(p => p.name == "regular").map(_.value)
    if (reg.nonEmpty) if (stringToBoolean(reg.head)) 1 else 2
    else 0
  }

  def stringToBoolean(s: String): Boolean = if (s == "true") true else false
  def stripPropertyName(s: String): String = s.stripPrefix("\"").stripSuffix("\"")

}
