scalaVersion := "2.11.7"

autoCompilerPlugins := true

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)

libraryDependencies ++= Seq(
  "com.softwaremill.scalamacrodebug" %% "macros" % "0.4")

libraryDependencies += "com.googlecode.kiama" %% "kiama" % "1.8.0"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.3.1" % "test"

testFrameworks += new TestFramework("utest.runner.Framework")
