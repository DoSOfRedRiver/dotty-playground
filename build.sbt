val dottyVersion = "0.17.0-RC1"

lazy val root = project
  .enablePlugins(GraalVMNativeImagePlugin)
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",

    mainClass in Compile := Some("mains.IOTests"),

    scalaVersion := dottyVersion,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
  )
