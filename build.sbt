lazy val `fuse-jna` = project settings (
           crossPaths := false,
  libraryDependencies += "net.java.dev.jna" % "jna" % "4.2.1"
)

lazy val idfs = project in file(".") dependsOn `fuse-jna` settings (
                       name :=  "idfs",
                description :=  "bidirectional passthrough fuse filesystem",
               organization :=  "org.improving",
   scalacOptions in compile ++= Seq("-Ywarn-unused", "-Ywarn-unused-import"),
 initialCommands in console :=  "import suffuse._, jio._",
        libraryDependencies +=  "com.novocode" % "junit-interface" % "0.11" % "test",
                   licenses :=  Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
               scalaVersion :=  "2.11.8",
                logBuffered :=  false,
                  maxErrors :=  10,
           triggeredMessage :=  Watched.clearWhenTriggered
)
