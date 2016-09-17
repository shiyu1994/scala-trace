lazy val commonSettings = Seq(
  organization := "cn.edu.sjtu.seiee.adapt",
  version := "1.0.0",
  scalaVersion := "2.11.7"
)

lazy val plugin = (project in file("scala-trace-plugin")).
  settings(commonSettings: _*).
  settings(
    name := "scala-trace-plugin",
    libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.11.7"
  ).dependsOn(runtime)

lazy val runtime = (project in file("scala-trace-runtime")).
  settings(commonSettings: _*).
  settings(
    name := "scala-trace-runtime"
  )

lazy val root = (project in file(".")).
  aggregate(plugin, runtime).
  settings(commonSettings: _*).
  settings(
    name := "scala-trace",
    libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.11.7"
  )


