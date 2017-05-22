import sbt._
import sbt.Keys._
import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import com.typesafe.sbteclipse.plugin.EclipsePlugin.EclipseKeys

// settings

val buildSettings = Defaults.coreDefaultSettings ++ Seq(
  organization := "nl.hugo",
  version      := "0.0.1",
  scalaVersion := "2.11.8",

  // compiler settings
  scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation", "-encoding", "utf8"),
  javacOptions ++= Seq("-Xlint:unchecked", "-Xlint:deprecation", "-encoding", "utf8"),

  // eclipse settings
  EclipseKeys.withSource := true,
  EclipseKeys.eclipseOutput := Some("target/eclipse"),
  unmanagedSourceDirectories in Compile ~= { _.filter(_.exists) },
  unmanagedSourceDirectories in Test ~= { _.filter(_.exists) }
)

val formattingPreferences = {
  import scalariform.formatter.preferences._
  FormattingPreferences()
    .setPreference(CompactControlReadability, false) // seems to work opposite
    .setPreference(DoubleIndentClassDeclaration, false)
    .setPreference(MultilineScaladocCommentsStartOnFirstLine, true)
    .setPreference(SpacesAroundMultiImports, true)
    .setPreference(PlaceScaladocAsterisksBeneathSecondAsterisk, true)
}
val formattingSettings = SbtScalariform.scalariformSettings ++ Seq(
  ScalariformKeys.preferences in Compile := formattingPreferences,
  ScalariformKeys.preferences in Test := formattingPreferences
)

lazy val projectSettings = buildSettings ++ formattingSettings

// versions

val typesafeRepo = "Typesafe Releases" at "http://dl.bintray.com/typesafe/ivy-releases"

val specs2Core = "org.specs2" %% "specs2-core" % "2.4.17" % "test"
val specs2Matcher = "org.specs2" %% "specs2-matcher" % "2.4.17" % "test"
val scalaTest = "org.scalatest" %% "scalatest" % "2.2.6" % "test"
val scalactic = "org.scalactic" %% "scalactic" % "2.2.6" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"

// dependencies

val dependencies = Seq(
  scalaTest,
  scalactic
)

// projects

lazy val rootProject = Project(
  id = "FunctionalProgramming",
  base = file("."),
  settings = projectSettings ++ Seq(
    libraryDependencies ++= dependencies
  )
)
