package xyz.discretezoo.core.util

import com.typesafe.config.{Config, ConfigFactory}

object DZConfig {

  private val config: Config = ConfigFactory.load()

  val externalResourcesGAP: String = config.getString("dz.paths.gap.external")
  val outputResultsGAP: String = config.getString("dz.paths.gap.output")
  val outputCodeMAGMA: String = config.getString("dz.paths.magma.input")
  val outputResultsMAGMA: String = config.getString("dz.paths.magma.output")

}
