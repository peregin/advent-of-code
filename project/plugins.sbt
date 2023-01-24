resolvers += "Sonatype Repository" at "https://oss.sonatype.org/content/groups/public"

// check latest updates for the dependencies
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.6.4")

// add Kotlin support
addSbtPlugin("com.hanhuy.sbt" % "kotlin-plugin" % "2.0.0")

