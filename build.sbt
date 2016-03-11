

lazy val `fuse-jna` = project settings (
           crossPaths := false,
  libraryDependencies += "net.java.dev.jna" % "jna" % "4.2.1"
)

lazy val idfs = project in file(".") dependsOn `fuse-jna` settings (
             name :=  "idfs",
      description :=  "bidirectional passthrough fuse filesystem",
     organization :=  "org.improving",
         licenses :=  Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
     scalaVersion :=  "2.11.8",
      logBuffered :=  false,
             test <<= run in Test toTask "",
        maxErrors :=  10,
 triggeredMessage :=  Watched.clearWhenTriggered
)
