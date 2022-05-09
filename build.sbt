lazy val compilerOptions =
  "-encoding" :: "UTF-8" ::
  "-feature" ::
  "-deprecation" ::
  "-unchecked" ::
  "-language:_" ::
  Nil

lazy val buildSettings =
  (organization := "io.github.sungiant") ::
  (logLevel := Level.Info) ::
  (outputStrategy := Some (StdoutOutput)) ::
  (scalaVersion := "3.1.0") :: Nil

lazy val commonSettings =
  (scalacOptions ++= compilerOptions) ::
  (ThisBuild / parallelExecution := false) :: 
  (resolvers += "Sonatype" at "https://oss.sonatype.org/content/repositories/releases/") ::
  (resolvers += "Sonatype Public" at "https://oss.sonatype.org/content/groups/public/") ::
  (resolvers += "Typesafe" at "https://repo.typesafe.com/typesafe/releases/") ::
  Nil

lazy val root = project
  .in (file ("src"))
  .settings (moduleName := "lmake")
  .settings (buildSettings)
  .settings (commonSettings)
  .settings (autoCompilerPlugins := true)
  .settings (libraryDependencies ++=
    "org.typelevel"     %% "cats-core"     % "2.6.1" ::
    "org.typelevel"     %% "cats-effect"   % "3.2.9" ::
    "io.circe"          %% "circe-core"    % "0.14.1" ::
    "io.circe"          %% "circe-generic" % "0.14.1" ::
    "io.circe"          %% "circe-parser"  % "0.14.1" ::
    "commons-io"        %  "commons-io"    % "2.11.0" ::
    "com.github.scopt"  %% "scopt"         % "4.0.1" ::
    Nil)
  .settings (assembly / mainClass := Some("LambdaMake"))
  .settings (assembly / assemblyJarName := "lmake")
  .settings (assembly / assemblyOutputPath := file(s"lmake.jar"))