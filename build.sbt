scalaVersion := "2.13.1"

name := "anscaml"
organization := "net.akouryy"
version := "2.1"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3",
  "com.lihaoyi" %% "pprint" % "0.5.6",
  "com.github.scopt" %% "scopt" % "4.0.0-RC2",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
)


// TIP: To find the "dependency" that you need to add to the
// `libraryDependencies` set, which in the above example looks like this:

// "org.typelevel" %% "cats-core" % "2.0.0"

// You can use Scaladex, an index of all known published Scala libraries. There,
// after you find the library you want, you can just copy/paste the dependency
// information that you need into your build file. For example, on the
// typelevel/cats Scaladex page,
// https://index.scala-lang.org/typelevel/cats, you can copy/paste the sbt
// dependency from the sbt box on the right-hand side of the screen.

// IMPORTANT NOTE: while build files look _kind of_ like regular Scala, it's
// important to note that syntax in *.sbt files doesn't always behave like
// regular Scala. For example, notice in this build file that it's not required
// to put our settings into an enclosing object or class. Always remember that
// sbt is a bit different, semantically, than vanilla Scala.

scalacOptions ++= Seq(
  "-encoding", "UTF-8",
  "-unchecked",
  "-deprecation",
  "-Xlint:unused",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Ywarn-unused:-patvars,_", // https://github.com/scala/bug/issues/10394#issuecomment-363086591

  "-feature",
  "-language:implicitConversions",

  // "-Ymacro-debug-lite",
)
