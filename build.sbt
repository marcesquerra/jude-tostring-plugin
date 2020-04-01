lazy val scala211 = "2.11.12"

ThisBuild / organization := "com.bryghts.jude"
ThisBuild / description := "jude - tostring plugin"

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

lazy val importer = (project in file("."))
 .enablePlugins(GitVersioning)
 .settings(
      name := "jude-tostring"
    , scalaVersion := scala211
    , publishMavenStyle := true
    , bintrayRepository := "jude"
    , bintrayOrganization := Some("bryghts")
    , git.useGitDescribe := true
    , git.formattedShaVersion := git.gitHeadCommit.value map { sha => s"v${sha.take(5).toUpperCase}" }
    , libraryDependencies += "org.scala-lang" % "scala-compiler" % scala211
 )


