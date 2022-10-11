// gallia-medium-article

// ===========================================================================
lazy val root = (project in file("."))
  .settings(
    name                 := "gallia-medium-article",
    version              := "0.1.0",
    scalaVersion         := "2.13.9")

// ---------------------------------------------------------------------------
Compile/mainClass := Some("galliamedium.initech.InitechGallia")

// ---------------------------------------------------------------------------
libraryDependencies += "io.github.galliaproject" %% "gallia-core" % "0.4.0"

// ===========================================================================
