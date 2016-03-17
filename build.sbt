lazy val `fuse-jna` = project in file("ext/fuse-jna") settings (
           crossPaths := false,
  libraryDependencies += "net.java.dev.jna" % "jna" % "4.2.1"
)

lazy val suffuse = ( project in file(".")
  aggregate (core)
  dependsOn (core)
  settings common settings (
    initialCommands in console := "import sfs._, jio._"
  )
)

lazy val core = project dependsOn `fuse-jna` settings common

def common = Seq[Setting[_]](
                organization :=  "org.improving",
    scalacOptions in compile ++= Seq("-Ywarn-unused", "-Ywarn-unused-import"),
         libraryDependencies +=  "com.novocode" % "junit-interface" % "0.11" % "test",
                    licenses :=  Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
                scalaVersion :=  "2.11.8",
                 logBuffered :=  false,
                   maxErrors :=  15,
            triggeredMessage :=  Watched.clearWhenTriggered
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")
