package suffuse
package fs

import jio._

/** Forwarding filesystem which maps files in a readonly sense.
 *
 *  Example:
 *
 *    > npm install -g json2yaml
 *    > sbt runMain suffuse.fs.mapfs /home/eecolor/test-src/ /home/eecolor/test-mnt/ json yaml json2yaml
 *    > echo '{ "test": [1, 2, "3"] }' > /home/eecolor/test-src/test.json
 *    > cd /home/eecolor/test-mnt/
 *    > ls
 *        test.yaml
 *    > cat test.yaml
 *        ---
 *          test:
 *            - 1
 *            - 2
 *            - "3"
 */
class MappedFs(
  protected val underlying: FuseFilesystem,
  map: Path => Metadataish
) extends ForwarderFs {

  override def readdir(path: String, df: DirectoryFiller): Int = {
    underlying.readdir(path, new MappedDirFiller(df, asPath andThen map andThen (path + _.fileName)))
  }

  override def getattr(path: String, stat: StatInfo): Int = {
    map(resolvePath(path)) match {
      case NoMetadata => doesNotExist()
      case metadata   => effect(eok)(populateStat(stat, metadata))
    }
  }

  override def read(path: String, buf: ByteBuffer, size: Long, offset: Long, info: FileInfo): Int =
    writeData(into = buf, data = map(resolvePath(path)).allBytes, amount = size, offset)
}

class MappedDirFiller(filler: DirectoryFiller, mapPath: String => String) extends DirectoryFiller {
  def add(files: jIterable[String]): Boolean = filler add (files.asScala map mapPath).asJava
  def add(files: String*): Boolean           = filler add (files map mapPath).asJava
}

object mapfs {
  def main(args: Array[String]): Unit = args.toList match {
    case from :: to :: sourceExt :: targetExt :: command :: Nil =>
      idfs(asPath(from)) map extensionsWithCommand(sourceExt, targetExt, command) mountForeground asPath(to)
    case _ =>
      println("Usage: map <from> <to> <fromExtension> <toExtension> <command>")
  }

  private def extensionsWithCommand(sourceExt: String, targetExt: String, command: String): Path => Metadataish = {
    case p if p.extension == sourceExt =>
      val target = p.replaceExtension(targetExt)
      materialize(target, data = convert(p, command), source = p)

    case p if p.extension == targetExt =>
      val source = p.replaceExtension(sourceExt)
      materialize(p, data = convert(source, command), source)

    case p if p.exists => p

    case _ => NoMetadata
  }

  private def materialize(path: Path, data: Array[Byte], source: Path) =
    Metadata(
      Node.File, path.fileName, data,
      source.permissions, source.atime, source.mtime
    )

  private def convert(path: Path, command: String): Array[Byte] =
    exec(command, path.to_s).stdout
}
