lazy val root = (project in file(".")).
  settings(
    name := "oscar-kep-propagator",
    version := "1.0",
    scalaVersion := "2.12.8",
    javaOptions in run += "-Xmx8G",
    resolvers += "Oscar Snapshots" at "http://artifactory.info.ucl.ac.be/artifactory/libs-snapshot/",
    resolvers += Resolver.sonatypeRepo("public"),
    libraryDependencies += "oscar" %% "oscar-algo" % "4.1.0-SNAPSHOT" withSources(),    
    libraryDependencies += "oscar" %% "oscar-cp" % "4.1.0-SNAPSHOT" withSources(),
    libraryDependencies += "com.github.scopt" % "scopt_2.12" % "4.0.0-RC2",
    libraryDependencies += "cplex" % "cplex" % "12.8" from 
    	"file://" + sys.env("CPLEX_STUDIO_HOME") + "/cplex/lib/cplex.jar",
  ).enablePlugins(PackPlugin)
  .settings(PackPlugin.packSettings)

