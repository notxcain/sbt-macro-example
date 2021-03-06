// scala.meta macros are at the moment only supported in 2.11.
scalaVersion in ThisBuild := "2.11.8"
scalaOrganization in ThisBuild := "org.typelevel"

scalacOptions ++= compilerOptions

// To find the latest version, see MetaVersion in https://github.com/scalameta/paradise/blob/master/build.sbt
lazy val metaVersion = "1.4.0"
// To find the latest PR number, see https://github.com/scalameta/paradise/commits/master
lazy val latestPullRequestNumber = 140
lazy val paradiseVersion = s"3.0.0-beta4"

lazy val compilerOptions = Seq[String](
  "-Ypartial-unification",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:experimental.macros"
)

lazy val metaMacroSettings: Seq[Def.Setting[_]] = Seq(
  // New-style macro annotations are under active development.  As a result, in
  // this build we'll be referring to snapshot versions of both scala.meta and
  // macro paradise.
  resolvers += Resolver.url("scalameta", url("http://dl.bintray.com/scalameta/maven"))(
    Resolver.ivyStylePatterns
  ),
  libraryDependencies += "org.scalameta" %% "scalameta" % metaVersion,
  libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2",
  libraryDependencies += "org.typelevel" %% "cats" % "0.8.1",
  libraryDependencies += compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
  sources in (Compile, doc) := Nil,
  // A dependency on macro paradise 3.x is required to both write and expand
  // new-style macros.  This is similar to how it works for old-style macro
  // annotations and a dependency on macro paradise 2.x.
  addCompilerPlugin("org.scalameta" % "paradise" % paradiseVersion cross CrossVersion.full),
  scalacOptions += "-Xplugin-require:macroparadise",
  // temporary workaround for https://github.com/scalameta/paradise/issues/10
  scalacOptions in (Compile, console) := Seq(), // macroparadise plugin doesn't work in repl yet.
  // temporary workaround for https://github.com/scalameta/paradise/issues/55
  sources in (Compile, doc) := Nil // macroparadise doesn't work with scaladoc yet.
)

// Define macros in this project.
lazy val macros = project.settings(
  metaMacroSettings,
  // A dependency on scala.meta is required to write new-style macros, but not
  // to expand such macros.  This is similar to how it works for old-style
  // macros and a dependency on scala.reflect.
  libraryDependencies += "org.scalameta" %% "scalameta" % "1.4.0.544",
  organization := "io.aecor"
)

// Use macros in this project.
lazy val app = project
  .settings(metaMacroSettings)
  .dependsOn(macros)
  .settings(Seq(scalacOptions ++= compilerOptions))
