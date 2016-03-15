package suffuse
package fs

import jio._

/** Forwarding filesystem which maps files in a readonly sense.
 */
class MappedFs(
  protected val underlying: FuseFilesystem,
  map: Path => Metadataish
) extends ForwarderFs {

  override def readdir(path: String, df: DirectoryFiller): Int =
    underlying.readdir(path, new MappedDirFiller(df, resolvePath andThen map andThen (_.fileName)))

  override def getattr(path: String, stat: StatInfo): Int = {
    resolvePath(path) match {
      case p if p.exists =>
        map(p) match {
          case NoMetadata => doesNotExist()
          case metadata   => effect(eok)(populateStat(stat, metadata))
        }
      case _ => doesNotExist()
    }
  }

  override def read(path: String, buf: ByteBuffer, size: Long, offset: Long, info: FileInfo): Int =
    writeData(into = buf, data = map(resolvePath(path)).allBytes, amount = size, offset)
}

class MappedDirFiller(filler: DirectoryFiller, mapName: String => String) extends DirectoryFiller {
  def add(files: jIterable[String]): Boolean = filler add (files.asScala map mapName).asJava
  def add(files: String*): Boolean           = filler add (files map mapName).asJava
}

object mapfs {
  def main(args: Array[String]): Unit = args.toList match {
    case from :: to :: sourceExt :: targetExt :: command :: Nil =>
      idfs(path(from)) map extensionsWithCommand(sourceExt, targetExt, command) mountForeground path(to)
    case _ =>
      println("Usage: map <from> <to> <fromExtension> <toExtension> <command>")
  }

  private def extensionsWithCommand(sourceExt: String, targetExt: String, command: String): Path => Metadataish = {
    case p if p.extension ==  sourceExt =>
      val target = p.replaceExtension(targetExt)
      materialize(target, data = convert(p, command), source = p)

    case p if p.extension == targetExt =>
      val source = p.replaceExtension(sourceExt)
      materialize(p, data = convert(source, command), source)

    case p => p
  }

  private def materialize(path: Path, data: Array[Byte], source: Path) =
    Metadata(
      Node.File, path.fileName, data,
      source.permissions, source.atime, source.mtime
    )

  private def convert(path: Path, command: String): Array[Byte] =
    exec(command, path.to_s).stdout
}
