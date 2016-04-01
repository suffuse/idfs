package sfs

import java.nio.ByteBuffer
import java.nio.{ file => jnf }
import java.nio.{ channels => jnc }
import jnf.{ attribute => jnfa }
import jnfa.PosixFilePermissions.asFileAttribute
import jnf.{ Files }
import jnf.LinkOption.NOFOLLOW_LINKS
import jnf.StandardCopyOption.REPLACE_EXISTING
import java.util.concurrent.TimeUnit
import javax.naming.SizeLimitExceededException
import scala.collection.convert.{ DecorateAsScala, DecorateAsJava }
import api._, attributes.UnixPerms.toBitSet

package object jio extends DecorateAsScala with DecorateAsJava with Alias {
  val UTF8          = java.nio.charset.Charset forName "UTF-8"

  def createTempDirectory(prefix: String): Path = Files.createTempDirectory(prefix)
  def homeDir: Path                             = path(sys.props("user.home"))
  def jList[A](xs: A*): jList[A]                = doto(new java.util.ArrayList[A])(xs foreach _.add)
  def jSet[A](xs: A*): jSet[A]                  = doto(new java.util.HashSet[A])(xs foreach _.add)
  def path: String => Path                      = jnf.Paths get _

  implicit class JioFilesInstanceOps(path: Path) extends JioFilesInstance(path)

  implicit class StreamOps[A](val xs: jStream[A]) extends AnyVal {
    def toVector: Vector[A] = {
      val buf = Vector.newBuilder[A]
      xs.iterator.asScala foreach (x => buf += x)
      buf.result
    }
  }

  implicit class PathOps(val path: Path) {
    def /(name: String): Path      = path resolve name

    def ls: Vector[Path]           = if (path.nofollow.isDirectory) withDirStream(path)(_.toVector) else Vector()
    def mkdir(bits: Long): Path    = path createDirectory asFileAttribute(toJavaPermissions(bits))
    def mkfile(bits: Long): Path   = path createFile asFileAttribute(toJavaPermissions(bits))
    def mklink(target: Path): Path = path createSymbolicLink target
    def readlink: Path             = path.readSymbolicLink
    def lastSegment: Name          = Option(path.getFileName) map (_.to_s) getOrElse ""

    def uid: Int                         = path.nofollow.getAttribute("unix:uid").asInstanceOf[Int]
    def gid: Int                         = path.nofollow.getAttribute("unix:gid").asInstanceOf[Int]
    def group: GroupPrincipal            = posixAttributes.group
    def inum: Object                     = basicAttributes.fileKey
    def owner: UserPrincipal             = posixAttributes.owner
    def perms: jSet[PosixFilePermission] = posixAttributes.permissions
    def nlink: Int                       = path.nofollow.getAttribute("unix:nlink").asInstanceOf[Int]

    def birth: FileTime = basicAttributes.creationTime
    def atime: FileTime = basicAttributes.lastAccessTime
    def ctime: FileTime = path.nofollow.getAttribute("unix:ctime").asInstanceOf[FileTime]
    def mtime: FileTime = basicAttributes.lastModifiedTime

    def isDir: Boolean   = path.nofollow.isDirectory
    def isFile: Boolean  = path.nofollow.isRegularFile
    def isLink: Boolean  = path.isSymbolicLink
    def isOther: Boolean = basicAttributes.isOther

    private def withDirStream[A](dir: Path)(code: jStream[Path] => A): A =
      (Files list dir) |> (str => try code(str) finally str.close())

    def attributes[A <: BasicFileAttributes : CTag](): Try[A] = Try(path.nofollow readAttributes classOf[A])
    def posixAttributes: PosixFileAttributes                  = attributes[PosixFileAttributes] | ??? // FIXME
    def basicAttributes: BasicFileAttributes                  = attributes[BasicFileAttributes] | ??? // FIXME

    def blockCount: Long     = (path.size + blockSize - 1) / blockSize
    def blockSize: Long      = 512 // FIXME
    def depth: Int           = path.getNameCount
    def extension: String    = filename.extension
    def filename: String     = lastSegment
    def mediaType: MediaType = MediaType(exec("file", "--brief", "--mime", "--dereference", to_s).stdout mkString "\n")
    def moveTo(target: Path) = path.nofollow move (target, REPLACE_EXISTING)
    def to_s: String         = path.toString

    def append(other: Path) = jio.path(path.to_s + other.to_s)

    def replaceExtension(newExtension: String): Path = path.resolveSibling(filename replaceExtension newExtension)

    def tryLock():jnc.FileLock     = withWriteChannel(_.tryLock)
    def truncate(size: Long): Unit = withWriteChannel {
      case c if c.size > size => c truncate size
      case c if c.size < size => c appendNullBytes (at = c.size, amount = (size - c.size).toInt)
                                 if (c.size < size) throw new SizeLimitExceededException
      case _                  => // sizes are equal
    }

    private def withWriteChannel[A](code: jnc.FileChannel => A): A = {
      val channel = jnc.FileChannel.open(path, jnf.StandardOpenOption.WRITE)
      try code(channel)
      finally channel.close()
    }
  }

  implicit class FileChannelOps(val c: FileChannel) extends AnyVal {
    def appendNullBytes(at: Long, amount: Int): Unit = {
      val nullBytes = Array.fill(amount)(0.toByte)
      c write (ByteBuffer wrap nullBytes, at)
    }
  }

  def toJavaPermissions(mask: Long): jFilePermissions = (toBitSet(mask) map UnixToJava).asJava

  def toUnixMask(perms: jFilePermissions) =
    ( perms.asScala map JavaToUnix ).foldLeft(0L)(_ | _)

  lazy val UnixToJava = (api.attributes.UnixPerms.Bits zip JavaBits).toMap
  lazy val JavaToUnix = (JavaBits zip api.attributes.UnixPerms.Bits).toMap

  lazy val JavaBits = {
    import jnfa.PosixFilePermission._
    Vector[PosixFilePermission](
      OWNER_READ,
      OWNER_WRITE,
      OWNER_EXECUTE,
      GROUP_READ,
      GROUP_WRITE,
      GROUP_EXECUTE,
      OTHERS_READ,
      OTHERS_WRITE,
      OTHERS_EXECUTE
    )
  }
}
