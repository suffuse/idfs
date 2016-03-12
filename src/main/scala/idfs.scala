package suffuse

import net.fusejna._
import StructFlock.FlockWrapper
import StructFuseFileInfo.FileInfoWrapper
import StructStat.StatWrapper
import types.TypeMode.{ ModeWrapper, NodeType, IModeWrapper }
import jio._
import scala.util.Properties.isMac

object idfs {
  def apply(from: Path, to: Path): idfs = {
    val fs = new idfs(from, to)
    effect(fs) {
      scala.sys addShutdownHook {
        if (fs.isMounted) {
          // System.err.println(s"Shutdown hook unmounting ${fs.mountPoint}")
          fs.unmountTry()
        }
      }
    }
  }

  def main(args: Array[String]): Unit = args.toList match {
    case from :: to :: Nil => idfs(path(from), path(to)).logging().mount()
    case from :: Nil       => idfs(path(from), path("/mnt")).logging().mount()
    case _                 => println("Usage: idfs <from> <to>")
  }
}

class idfs private (from: Path, to: Path) extends util.FuseFilesystemAdapterFull {
  val mountPoint: File = to.toFile.getAbsoluteFile

  override def getOptions() = Array("-o", "direct_io,default_permissions")

  def logging(): this.type = effect[this.type](this)(this log true)
  def mount(): Unit        = super.mount(mountPoint, false) // blocking=false
  def mountfg(): Unit      = super.mount(mountPoint, true) // blocking=true
  def unmountTry(): Unit   = (
    if (isMac)
      exec("umount", "-f", mountPoint.getPath) orElse exec("diskutil", "unmount", mountPoint.getPath)
    else
      exec("fusermount", "-u", mountPoint.getPath)
  )

  override def read(path: String, buf: ByteBuffer, size: Long, offset: Long, info: FileInfoWrapper): Int = {
    val p    = resolvePath(path)
    val data = p.allBytes
    effect(size.toInt)(
      if (offset + size > data.length)
        buf.put(data, offset.toInt, data.length - offset.toInt)
      else
        buf.put(data, offset.toInt, size.toInt)
    )
  }
  override def write(path: String, buf: ByteBuffer, size: Long, offset: Long, info: FileInfoWrapper): Int = {
    def impl(): Unit = {
      val arr = new Array[Byte](size.toInt)
      buf get arr
      val f = resolveFile(path)
      f appending (_ write arr)
    }
    Try(impl) fold (_ => -1, _ => size.toInt)
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
      val f = resolveFile(path)
      f.createNewFile()
      populateMode(mode, f.toPath, NodeType.FILE)
    }
  }
  override def mkdir(path: String, mode: ModeWrapper): Int = {
    resolveFile(path) match {
      case f if f.exists => alreadyExists()
      case f             => effect(eok) {
        f.mkdir()
        populateMode(mode, f.toPath, NodeType.DIRECTORY)
      }
    }
  }
  override def getattr(path: String, stat: StatWrapper): Int = {
    import NodeType._
    resolvePath(path) match {
      case f if f.isFile         => effect(eok)(populateStat(stat, f, FILE))
      case d if d.isDirectory    => effect(eok)(populateStat(stat, d, DIRECTORY))
      case l if l.isSymbolicLink => effect(eok)(populateStat(stat, l, SYMBOLIC_LINK))
      case _                     => doesNotExist()
    }
  }
  override def rename(from: String, to: String): Int = {
    tryFuse(resolveFile(from) renameTo resolveFile(to))
  }
  override def rmdir(path: String): Int = {
    resolvePath(path) match {
      case d if d.isDirectory => effect(eok)(d.deleteRecursive())
      case _                  => doesNotExist()
    }
  }
  override def unlink(path: String): Int = {
    resolveFile(path) match {
      case f if f.isFile => effect(eok)(f.delete())
      case _             => notImplemented()
    }
  }
  override def chmod(path: String, mode: ModeWrapper): Int = {
    resolvePath(path) match {
      case p if p.exists => effect(eok)(p.setPermissions(mode.mode))
      case _             => doesNotExist()
    }
  }

  private def getUID(): Long = getFuseContext.uid.longValue
  private def getGID(): Long = getFuseContext.gid.longValue
  private def resolvePath(p: String): Path = path(s"$from$p")
  private def resolveFile(path: String): File = path match {
    case "/" => from.toFile
    case _   => new File(from.toFile, path stripSuffix "/")
  }

  private def populateStat(stat: StatWrapper, path: Path, nodeType: NodeType): Unit = {
    populateMode(stat, path, nodeType)
    stat size   path.size
    stat atime  path.atime
    stat mtime  path.mtime
    stat blocks path.blockCount
    stat nlink 1
    stat uid getUID
    stat gid getGID
  }

  private def populateMode(mode: IModeWrapper, path: Path, nodeType: NodeType): Unit = {
    val pp = path.permissions
    import pp._
    mode setMode (nodeType,
      ownerRead, ownerWrite, ownerExecute,
      groupRead, groupWrite, groupExecute,
      otherRead, otherWrite, otherExecute
    )
  }
}
