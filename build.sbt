lazy val commonSettings = Seq(
  organization := "cn.edu.sjtu.seiee.adapt.scala",
  version := "0.1.0",
  scalaVersion := "2.11.7"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "ScalaTrace",
    libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.11.7"
  )