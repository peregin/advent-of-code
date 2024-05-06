

name := "advent-of-code"
organization := "velocorner.com"
version := "1.0.0"

scalaVersion := "3.3.1"
scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-explain",
  "-explain-types"
)

libraryDependencies += "com.github.simerplaha" % "slack3d_2.13" % "0.1.0" withSources () withJavadoc ()
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
//libraryDependencies += "org.typelevel" %% "cats-effect" % "3.4.2" withSources () withJavadoc ()

onLoadMessage := Def.setting {
  import scala.Console._
  def green(text: String): String = s"$GREEN$text$RESET"

  s"""|${green("""                                       """)}
      |${green("""                                       """)}
      |${green("""                _==_ _                 """)}
      |${green("""              _,(",)|_|                """)}
      |${green("""               ( . \-|                 """)}
      |${green("""             _(  :  )|_                """)}
      |${green("""                                       """)}
      |${green("""                                       """ + version.value)}
      |
      """.stripMargin
}.value
