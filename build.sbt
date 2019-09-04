organization := "mek"

javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

scalacOptions ++= Seq(
    "-encoding",
    "UTF-8", // yes, this is 2 args
    "-target:jvm-1.8",
    "-deprecation",
    "-feature",
    "-unchecked",
    "-Xlint",
    "-Yno-adapted-args",
    "-Ywarn-numeric-widen"
)

scalacOptions in Test ++= Seq("-Yrangepos")

autoAPIMappings := true

scalaVersion := "2.11.12"

libraryDependencies ++= {
    Seq(
        "org.scalatest" %% "scalatest" % "3.0.5" % Test
    )
}

name := "exchange"
