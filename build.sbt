name := "MiniCalc"
version := "0.1.0"
scalaVersion := "3.3.6"

libraryDependencies ++= Seq(
  // Core
  "org.typelevel" %% "cats-effect" % "3.5.7",
  
  // Testing
  "org.scalatest" %% "scalatest" % "3.2.18" % Test
)

// Recommended settings for Scala 3
scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked"
)