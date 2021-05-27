resolvers += "Sonatype Repository" at "https://oss.sonatype.org/content/groups/public"

// to generate dependency graph of the libraries
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.9.2")

// check latest updates for the dependencies
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.5.3")

