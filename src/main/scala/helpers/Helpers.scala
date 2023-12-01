package helpers

import zio.*
import zio.ZIO.*
import zio.stream.*
import zio.nio.*
import zio.nio.file.*
import zio.nio.charset.*
import zio.stream.*

object Helpers {

  def fileLines(path: Path, charset: Charset = Charset.Standard.utf8): Task[List[String]] =
    Files.readAllLines(path, charset)

  def fileLinesStream(path: Path, charset: Charset = Charset.Standard.utf8): Stream[Throwable, String] =
    ZStream
      .fromFile(path.toFile)
      .via(ZPipeline.decodeStringWith(charset.javaCharset) >>> ZPipeline.splitLines)

  def fileContent(path: Path, charset: Charset = Charset.Standard.utf8): Task[String] = for {
    bytes   <- Files.readAllBytes(path)
    content <- charset.decodeChunk(bytes)
  } yield content.mkString

}
