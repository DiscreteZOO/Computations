name := "discretezoo-core"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(

  // slick dependencies
  "com.typesafe.slick" %% "slick" % "3.2.3",
  "org.slf4j" % "slf4j-nop" % "1.6.4",
  "com.typesafe.slick" %% "slick-hikaricp" % "3.2.3",
  "org.postgresql" % "postgresql" % "42.2.2",

  // arrays for slick
  "com.github.tminglei" %% "slick-pg" % "0.16.2",

  // grammar parser
  "org.parboiled" %% "parboiled" % "2.1.4",

  // unit tests
  "org.scalactic" %% "scalactic" % "3.0.5",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"

)