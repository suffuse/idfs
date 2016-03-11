package suffuse

import net.fusejna._
import StructFlock.FlockWrapper
import StructFuseFileInfo.FileInfoWrapper
import StructStat.StatWrapper
import types.TypeMode.{ ModeWrapper, NodeType }

object idfs {
  def main(args: Array[String]): Unit = args.toList match {
    case from :: to :: Nil => new idfs(path(from), path(to)) mount
    case from :: Nil       => new idfs(path(from), path("/mnt")) mount
    case _                 => println("Usage: idfs <from> <to>")
  }
}

class idfs(from: Path, to: Path) extends util.FuseFilesystemAdapterFull {
  def mountPoint: String = to.toFile.getAbsolutePath
  def mount(): Unit = {
    log(true)
    super.mount(mountPoint)
  }

  override def read(path: String, buf: ByteBuffer, size: Long, offset: Long, info: FileInfoWrapper): Int = {
    val p    = resolvePath(path)
    val data = p.readAllBytes()
    also(size.toInt)(
      if (offset + size > data.length)
        buf.put(data, offset.toInt, data.length - offset.toInt)
      else
        buf.put(data, offset.toInt, size.toInt)
    )
  }
  override def lock(path: String, info: FileInfoWrapper, command: FlockCommand, flock: FlockWrapper): Int = {
    tryFuse(resolvePath(path).openChannel().tryLock())
  }
  override def readdir(path: String, filler: DirectoryFiller): Int = also(EOK) {
    resolveFile(path) match {
      case d if d.isDirectory => d.listFiles foreach (filler add _.toString)
      case _                  =>
    }
  }
  override def readlink(path: String, buf: ByteBuffer, size: Long): Int = {
    resolvePath(path) match {
      case p if !p.exists         => doesNotExist()
      case p if !p.isSymbolicLink => isNotValid()
      case p                      => also(EOK)(buf put (p.readSymbolicLink.toString getBytes UTF8))
    }
  }
  override def create(path: String, mode: ModeWrapper, info: FileInfoWrapper): Int = {
    tryFuse {
      resolveFile(path).createNewFile
      mode.setMode(NodeType.FILE, true, true, true)
    }
  }
  override def mkdir(path: String, mode: ModeWrapper): Int = {
    resolveFile(path) match {
      case f if f.exists => alreadyExists()
      case f             => also(EOK)(f.mkdir())
    }
  }
  override def getattr(path: String, stat: StatWrapper): Int = {
    resolveFile(path) match {
      case f if f.isFile      => also(EOK)(populateStat(stat, f))
      case d if d.isDirectory => also(EOK)(stat setMode NodeType.DIRECTORY)
      case _                  => doesNotExist()
    }
  }
  override def rename(from: String, to: String): Int = {
    tryFuse(resolveFile(from) renameTo resolveFile(to))
  }

  private def resolvePath(p: String): Path = path(s"$from$p")
  private def resolveFile(path: String): File = path match {
    case "/" => from.toFile
    case _   => new File(from.toFile, path stripSuffix "/")
  }
  private def populateStat(stat: StatWrapper, f: File): Unit = {
    stat setMode (NodeType.FILE, true, true, true, true, true, true, true, true, true)
    stat size f.length
    stat atime (f.lastModified / 1000L)
    stat mtime 0
    stat nlink 1
    stat uid 0
    stat gid 0
    stat blocks (f.length + 511L) / 512L
  }
}
