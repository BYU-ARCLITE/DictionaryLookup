name         := "dictionary"

version      := "1.0-SNAPSHOT"

scalaVersion := "2.11.7"

lazy val root = (project in file(".")).enablePlugins(PlayScala).settings(
  libraryDependencies ++= Seq(
    // Add your project dependencies here
    jdbc,
	cache,
	ws,
	evolutions,
    "mysql" % "mysql-connector-java" % "5.1.10",
    "org.mindrot" % "jbcrypt" % "0.3m",
    "org.scalaj" %% "scalaj-http" % "2.0.0",
	"com.typesafe.play" %% "anorm" % "2.4.0"
  )
)