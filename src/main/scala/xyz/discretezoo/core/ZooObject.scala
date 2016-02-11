package xyz.discretezoo.core

import _root_.io.duality.PersistableSet

/**
  * Created by katja on 05/01/16.
  */
trait ZooObject {

  val uniqueId: String
  val catalogues: PersistableSet[CatalogId]
  val aliases: PersistableSet[String]

  def description: String
  override def toString: String

}
