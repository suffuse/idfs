package suffuse
package fs

import jio._

object idfs {
  def apply(from: Path): idfs = new idfs(from)
  def main(args: Array[String]): Unit = args.toList match {
    case from :: to :: Nil => idfs(asPath(from)).logging() mountForeground asPath(to)
    case _                 => println("Usage: idfs <from> <to>")
  }
}

class idfs private (from: Path) extends FuseFsFull {
  override def read(path: String, buf: ByteBuffer, size: Long, offset: Long, info: FileInfo): Int = {
    val p    = resolvePath(path)
    writeData(into = buf, data = p.allBytes, amount = size, offset)
  }
  override def write(path: String, buf: ByteBuffer, size: Long, offset: Long, info: FileInfo): Int = {
    def impl(): Unit = {
      val arr = new Array[Byte](size.toInt)
      buf get arr
      val f = resolveFile(path)
      f appending (_ write arr)
    }
    Try(impl) fold (_ => -1, _ => size.toInt)
  }

  override def lock(path: String, info: FileInfo, command: FlockCommand, flock: FlockWrapper): Int = {
    tryFuse(resolvePath(path).tryLock())
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
      case p                      => effect(eok)(buf put (p.readlink.to_s getBytes jio.UTF8))
    }
  }
  override def create(path: String, mode: ModeInfo, info: FileInfo): Int = {
    import Node._
    val p = resolvePath(path)
    mode.`type`() match {
      case Dir             => tryFuse(p mkdir mode.mode)
      case File            => tryFuse(p mkfile mode.mode)
      case Fifo | Socket   => notSupported()
      case BlockDev | Link => notSupported()
    }
  }
  override def mkdir(path: String, mode: ModeInfo): Int = {
    resolvePath(path) match {
      case f if f.exists => alreadyExists()
      case f             => effect(eok)(f.mkdir(mode.mode))
    }
  }
  override def getattr(path: String, stat: StatInfo): Int = {
    resolvePath(path) match {
      case x if x.isKnownType => effect(eok)(populateStat(stat, x))
      case _                  => doesNotExist()
    }
  }
  override def rename(from: String, to: String): Int = {
    tryFuse(resolvePath(from) moveTo resolvePath(to))
  }
  override def rmdir(path: String): Int = tryFuse {
    resolvePath(path) match {
      case d if d.isDirectory => effect(eok)(d.delete())
      case _                  => doesNotExist()
    }
  }
  override def unlink(path: String): Int = {
    resolvePath(path) match {
      case f if f.exists => effect(eok)(f.delete())
      case _             => doesNotExist()
    }
  }
  override def chmod(path: String, mode: ModeInfo): Int = {
    resolvePath(path) match {
      case p if p.exists => effect(eok)(p.setPermissions(mode.mode))
      case _             => doesNotExist()
    }
  }
  override def symlink(target: String, linkName: String): Int = {
    tryFuse {
      resolvePath("/" + linkName) mklink asPath(target)
    }
  }
  override def link(from: String, to: String): Int = {
    notSupported()
  }
  override def truncate(path: String, size: Long): Int = tryFuse {
    effect(eok)(resolvePath(path) truncate size)
  }
  override def utimens(path: String, wrapper: TimeBufferWrapper) = {
    tryFuse(resolvePath(path) setLastModifiedTime wrapper.mod_nsec)
  }

  def resolvePath: String => Path = p => asPath(s"$from$p")
  private def resolveFile(path: String): File = path match {
    case "/" => from.toFile
    case _   => new File(from.toFile, path stripSuffix "/")
  }
}
