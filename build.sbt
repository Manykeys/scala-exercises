

name := "exercises"

version := "0.1"

inThisBuild {
  List(
    scalaVersion := "2.13.12",
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    scalafixScalaBinaryVersion := (ThisBuild / scalaBinaryVersion).value,
    scalafixDependencies ++= Seq(
      "com.github.vovapolu" %% "scaluzzi" % "0.1.23"
    )
  )
}

val libraries = Seq(
  "org.scalatest"     %% "scalatest"       % "3.2.3"         % Test,
  "org.scalatestplus" %% "scalacheck-1-15" % "3.3.0.0-SNAP3" % Test
)

// exercises
lazy val exercises00 = project in file("exercises00") settings (libraryDependencies ++= libraries)
lazy val exercises01 = project in file("exercises01") settings (libraryDependencies ++= libraries)
lazy val exercises02 = project in file("exercises02") settings (libraryDependencies ++= libraries)
lazy val exercises03 = project in file("exercises03") settings (
  libraryDependencies ++= libraries,
  scalacOptions ++= Seq("-feature", "-language:implicitConversions")
)
lazy val exercises04 = project in file("exercises04") settings (
  libraryDependencies ++= libraries,
  scalacOptions ++= Seq("-feature", "-language:implicitConversions")
)
lazy val exercises05 = project in file("exercises05") settings (
  libraryDependencies ++= libraries,
  scalacOptions ++= Seq("-feature", "-language:implicitConversions"),
  addCompilerPlugin(kindProjectorDep)
)
lazy val exercises06 = project in file("exercises06") settings (
  libraryDependencies ++= libraries,
  scalacOptions ++= Seq("-feature", "-language:implicitConversions"),
  addCompilerPlugin(kindProjectorDep)
)
lazy val exercises07 = project in file("exercises07") settings (
  libraryDependencies ++= libraries,
  scalacOptions ++= Seq("-feature", "-language:implicitConversions"),
  addCompilerPlugin(kindProjectorDep)
)
lazy val exercises08 = project in file("exercises08") settings (
  libraryDependencies ++= libraries,
  scalacOptions ++= Seq("-feature", "-language:implicitConversions"),
  libraryDependencies += "org.typelevel" %% "cats-core" % "2.10.0",
  addCompilerPlugin(kindProjectorDep)
)
lazy val exercises09 = project in file("exercises09") settings (
  libraryDependencies ++= libraries,
  scalacOptions ++= Seq("-feature", "-language:implicitConversions"),
  libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.4",
  libraryDependencies += "org.typelevel" %% "cats-effect-testing-scalatest" % "1.5.0" % Test,
  addCompilerPlugin(kindProjectorDep)
)

lazy val workshop = project in file("workshop") settings (
  scalacOptions ++= Seq("-feature", "-language:implicitConversions"),
  libraryDependencies ++= Seq(
    "com.github.pureconfig" %% "pureconfig" % "0.17.6",
    "org.xerial" % "sqlite-jdbc" % "3.45.3.0",
    "org.tpolecat" %% "doobie-core" % "1.0.0-RC4",
    "org.tpolecat" %% "doobie-hikari" % "1.0.0-RC4",
    "org.http4s" %% "http4s-ember-server" % "0.23.26",
    "org.http4s" %% "http4s-dsl" % "0.23.26",
    "org.http4s" %% "http4s-ember-client" % "0.23.26",
    "org.http4s" %% "http4s-circe" % "0.23.26",
    "io.circe" %% "circe-generic" % "0.14.6",
    "io.circe" %% "circe-literal" % "0.14.6",
    "io.circe" %% "circe-parser" % "0.14.6",
    "com.github.fd4s" %% "fs2-kafka" % "3.5.0",
    "ch.qos.logback" % "logback-classic" % "1.5.3",
    "org.scalatest" %% "scalatest" % "3.2.3" % Test,
    "org.scalamock" %% "scalamock" % "5.1.0" % Test,
    "org.typelevel" %% "cats-effect-testing-scalatest" % "1.5.0" % Test
  ),
  addCompilerPlugin(kindProjectorDep)
)

// lectures
lazy val lecture01 = project in file("lecture01") settings (libraryDependencies ++= libraries)
lazy val lecture02 = project in file("lecture02") settings (libraryDependencies ++= libraries)
lazy val lecture03 = project in file("lecture03") settings (libraryDependencies ++= libraries)
lazy val lecture04 = project in file("lecture04") settings (libraryDependencies ++= libraries)
lazy val lecture05 = project in file("lecture05") settings (libraryDependencies ++= libraries) settings addCompilerPlugin(kindProjectorDep)
lazy val lecture06 = project in file("lecture06") settings (libraryDependencies ++= libraries) settings addCompilerPlugin(kindProjectorDep)
lazy val lecture07 = project in file("lecture07") settings (libraryDependencies ++= libraries) settings addCompilerPlugin(kindProjectorDep)
lazy val lecture08 = project in file("lecture08") settings (libraryDependencies ++= libraries) settings addCompilerPlugin(kindProjectorDep)
lazy val lecture09 = project in file("lecture09") settings (libraryDependencies ++= libraries) settings addCompilerPlugin(kindProjectorDep)

lazy val kindProjectorDep = "org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full
