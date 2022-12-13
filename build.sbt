

name := "advent-of-code"
organization := "velocorner.com"
version := "1.0.0"

scalaVersion := "3.2.0"
scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-explain",
  "-explain-types"
)

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
