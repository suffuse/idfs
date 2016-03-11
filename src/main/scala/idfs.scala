package suffuse

import net.fusejna._
import StructFlock.FlockWrapper
import StructFuseFileInfo.FileInfoWrapper
import StructStat.StatWrapper
import types.TypeMode.{ ModeWrapper, NodeType }
import jio._

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
    effect(size.toInt)(
      if (offset + size > data.length)
        buf.put(data, offset.toInt, data.length - offset.toInt)
      else
        buf.put(data, offset.toInt, size.toInt)
    )
  }
  override def lock(path: String, info: FileInfoWrapper, command: FlockCommand, flock: FlockWrapper): Int = {
    tryFuse(resolvePath(path).openChannel().tryLock())
  }
  override def readdir(path: String, filler: DirectoryFiller): Int = effect(eok) {
    resolveFile(path) match {
      case d if d.isDirectory => d.listFiles foreach (filler add _.toString)
      case _                  =>
    }
  }
  override def readlink(path: String, buf: ByteBuffer, size: Long): Int = {
    resolvePath(path) match {
      case p if !p.exists         => doesNotExist()
      case p if !p.isSymbolicLink => isNotValid()
      case p                      => effect(eok)(buf put (p.readSymbolicLink.toString getBytes jio.UTF8))
    }
  }
  override def create(path: String, mode: ModeWrapper, info: FileInfoWrapper): Int = {
    tryFuse {
      resolveFile(path).createNewFile()
      mode.setMode(NodeType.FILE, true, true, true)
    }
  }
  override def mkdir(path: String, mode: ModeWrapper): Int = {
    resolveFile(path) match {
      case f if f.exists => alreadyExists()
      case f             => effect(eok)(f.mkdir())
    }
  }
  override def getattr(path: String, stat: StatWrapper): Int = {
    resolveFile(path) match {
      case f if f.isFile      => effect(eok)(populateStat(stat, f))
      case d if d.isDirectory => effect(eok)(stat setMode NodeType.DIRECTORY)
      case _                  => doesNotExist()
    }
  }
  override def rename(from: String, to: String): Int = {
    tryFuse(resolveFile(from) renameTo resolveFile(to))
  }

  private def getUID(): Long = getFuseContext.uid.longValue
  private def getGID(): Long = getFuseContext.gid.longValue
  private def resolvePath(p: String): Path = path(s"$from$p")
  private def resolveFile(path: String): File = path match {
    case "/" => from.toFile
    case _   => new File(from.toFile, path stripSuffix "/")
  }

  private def populateStat(stat: StatWrapper, f: File): Unit = {
    stat setMode (NodeType.FILE, true, true, true, true, true, true, true, true, true)
    stat size f.length
    stat atime f.mtime
    stat mtime f.mtime
    stat nlink 1
    stat uid getUID
    stat gid getGID
    stat blocks (f.length + 511L) / 512L
  }
}
