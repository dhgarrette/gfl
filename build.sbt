import com.typesafe.sbt.SbtStartScript

name := "gfl-scala"

version := "0.0.2-SNAPSHOT"

organization := "dhg"

scalaVersion := "2.11.6"

resolvers ++= Seq(
  "dhg releases repo" at "http://www.cs.utexas.edu/~dhg/maven-repository/releases",
  "dhg snapshot repo" at "http://www.cs.utexas.edu/~dhg/maven-repository/snapshots"
)

libraryDependencies ++= Seq(
   "dhg" % "scala-util_2.11" % "0.0.2-SNAPSHOT",
   "io.argonaut" %% "argonaut" % "6.0.4",
   "junit" % "junit" % "4.11" % "test",
   "com.novocode" % "junit-interface" % "0.10" % "test"
  )

seq(SbtStartScript.startScriptForClassesSettings: _*)

SbtStartScript.stage in Compile := Unit

scalacOptions ++= Seq("-deprecation")
