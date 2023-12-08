//enablePlugins(ScalaNativePlugin)

name := "advent-of-code-2023"

version := "0.1"

scalaVersion := "3.3.1"

lazy val versions = new {
  val zio       = "2.0.19"
  val nio       = "2.0.2"
  val fastparse = "3.0.2"
}

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse"    % versions.fastparse,
  "dev.zio"     %% "zio"          % versions.zio,
  "dev.zio"     %% "zio-nio"      % versions.nio,
  "dev.zio"     %% "zio-test"     % versions.zio,
  "dev.zio"     %% "zio-streams"  % versions.zio,
  "dev.zio"     %% "zio-test"     % versions.zio % Test,
  "dev.zio"     %% "zio-test-sbt" % versions.zio % Test
)

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
