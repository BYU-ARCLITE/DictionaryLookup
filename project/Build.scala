import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "dictionary"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    // Add your project dependencies here,
    jdbc,
    anorm,
    "mysql" % "mysql-connector-java" % "5.1.10",
    "org.mindrot" % "jbcrypt" % "0.3m",
    "org.scalaj" %% "scalaj-http" % "2.0.0"
  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    // Add your own project settings here

  )

}
