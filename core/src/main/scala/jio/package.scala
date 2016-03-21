package sfs

import java.nio.ByteBuffer
import java.nio.{ file => jnf }
import java.nio.{ channels => jnc }
import jnf.{ attribute => jnfa }
import jnf.{ Files }
import jnf.LinkOption.NOFOLLOW_LINKS
import jnfa.PosixFilePermissions.asFileAttribute
import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeUnit.SECONDS
import javax.naming.SizeLimitExceededException
import scala.collection.convert.{ DecorateAsScala, DecorateAsJava }
import api._

package object jio extends DecorateAsScala with DecorateAsJava {
  val UTF8          = java.nio.charset.Charset forName "UTF-8"
  val UnixUserClass = Class.forName("sun.nio.fs.UnixUserPrincipals$User")
  val UidMethod     = doto(UnixUserClass getDeclaredMethod "uid")(_ setAccessible true)

  def jList[A](xs: A*): jList[A] = doto(new java.util.ArrayList[A])(xs foreach _.add)
  def jSet[A](xs: A*): jSet[A]   = doto(new java.util.HashSet[A])(xs foreach _.add)

  implicit class JioFilesInstanceOps(path: Path) extends JioFilesInstance(path)

  implicit class StreamOps[A](val xs: jStream[A]) extends AnyVal {
    def toVector: Vector[A] = {
      val buf = Vector.newBuilder[A]
      xs.iterator.asScala foreach (x => buf += x)
      buf.result
    }
  }

  implicit class ClassOps[A](val c: Class[A]) {
    def shortName: String = (c.getName.stripSuffix("$") split "[.]").last split "[$]" last
  }

  implicit class FileOps(val f: File) extends Pathish[File] {
    def path: Path     = f.toPath
    def asRep(p: Path) = p.toFile

    def appending[A](g: FileOutputStream => A): A = {
      val stream = new FileOutputStream(f, true) // append = true
      try g(stream) finally stream.close()
    }
  }
  implicit class PathOps(val path: Path) extends Pathish[Path] {
    def asRep(p: Path) = p

    def append(other: Path) = jio.path(path.to_s + other.to_s)

    def permissions: PosixFilePermissions = {
      val pfp = (Files getPosixFilePermissions (path, NOFOLLOW_LINKS)).asScala
      import jnfa.PosixFilePermission._
      PosixFilePermissions(
        pfp(GROUP_READ) , pfp(GROUP_WRITE) , pfp(GROUP_EXECUTE),
        pfp(OWNER_READ) , pfp(OWNER_WRITE) , pfp(OWNER_EXECUTE),
        pfp(OTHERS_READ), pfp(OTHERS_WRITE), pfp(OTHERS_EXECUTE)
      )
    }

    def setPermissions(bits: Long): Unit =
      Files.setPosixFilePermissions(path, bitsAsPermissions(bits))

    def setLastModifiedTime(nanoSeconds: Long): Unit =
      Files.setLastModifiedTime(path, jnfa.FileTime.from(nanoSeconds, TimeUnit.NANOSECONDS))

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

  trait Pathish[Rep] {
    def path: Path
    def asRep(p: Path): Rep

    def /(name: String): Rep      = asRep(path resolve name)

    def ls: Vector[Rep]           = if (path.nofollow.isDirectory) withDirStream(path)(_.toVector map asRep) else Vector()
    def mkdir(bits: Long): Rep    = asRep(path createDirectory asFileAttribute(bitsAsPermissions(bits)))
    def mkfile(bits: Long): Rep   = asRep(path createFile asFileAttribute(bitsAsPermissions(bits)))
    def mklink(target: Path): Rep = asRep(path createSymbolicLink target)
    def readlink: Rep             = asRep(path.readSymbolicLink)

    def metadata: Metadata = {
      import jnfa.PosixFilePermission._
      import api.attributes._
      val metadata =
        Metadata(
          Atime(atime to SECONDS),
          Mtime(mtime to SECONDS),
          UnixPerms(toUnixMask(perms)),
          Uid(uid)
        )

           if (isFile) metadata set File set Size(path.size) set BlockCount(blockCount)
      else if (isDir ) metadata set Dir
      else if (isLink) metadata set Link
      else metadata
    }
    def uid: Int                         = (UidMethod invoke owner).asInstanceOf[Int]
    def gid: Int                         = 0 // OMG what a hassle.
    def atime: FileTime                  = basicAttributes.lastAccessTime
    def ctime: FileTime                  = basicAttributes.creationTime
    def group: GroupPrincipal            = posixAttributes.group
    def inum: Object                     = basicAttributes.fileKey
    def mtime: FileTime                  = basicAttributes.lastModifiedTime
    def owner: UserPrincipal             = posixAttributes.owner
    def perms: jSet[PosixFilePermission] = posixAttributes.permissions

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
    def filename: String     = path.getFileName.to_s
    def mediaType: MediaType = MediaType(exec("file", "--brief", "--mime", "--dereference", to_s).stdout mkString "\n")
    def moveTo(target: Path) = path.nofollow move target
    def to_s: String         = path.toString
  }

  case class PosixFilePermissions(
    groupRead: Boolean, groupWrite: Boolean, groupExecute: Boolean,
    ownerRead: Boolean, ownerWrite: Boolean, ownerExecute: Boolean,
    otherRead: Boolean, otherWrite: Boolean, otherExecute: Boolean
  )

  def file(s: String, ss: String*): File        = ss.foldLeft(new File(s))(new File(_, _))
  def path: String => Path                      = jnf.Paths get _
  def homeDir: Path                             = path(sys.props("user.home"))
  def createTempDirectory(prefix: String): Path = Files.createTempDirectory(prefix)

  implicit class UnixPermsOps(val perms: api.attributes.UnixPerms) extends AnyVal {
    def java: Set[PosixFilePermission] = perms.bits map UnixToJava
  }

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

  def bitsAsPermissions(bits: Long): jFilePermissions = {
    import jnfa.PosixFilePermission._
    val permissionBits = Set(
      (1 << 8, OWNER_READ),
      (1 << 7, OWNER_WRITE),
      (1 << 6, OWNER_EXECUTE),
      (1 << 5, GROUP_READ),
      (1 << 4, GROUP_WRITE),
      (1 << 3, GROUP_EXECUTE),
      (1 << 2, OTHERS_READ),
      (1 << 1, OTHERS_WRITE),
      (1 << 0, OTHERS_EXECUTE)
    )
    val permissions =
      permissionBits.foldLeft(Set.empty[PosixFilePermission]) {
        case (result, (bit, permission)) =>
          if ((bits & bit) == 0) result
          else result + permission
      }
    permissions.asJava
  }

  type jArray[A]        = Array[A with Object]
  type jClass           = java.lang.Class[_]
  type jField           = java.lang.reflect.Field
  type jFilePermissions = jSet[PosixFilePermission]
  type jIterable[+A]    = java.lang.Iterable[A @uV]
  type jIterator[+A]    = java.util.Iterator[A @uV]
  type jLineIterable    = jIterable[_ <: CharSequence]
  type jList[A]         = java.util.List[A]
  type jMap[K, V]       = java.util.Map[K, V]
  type jMethod          = java.lang.reflect.Method
  type jSet[A]          = java.util.Set[A]
  type jStream[+A]      = java.util.stream.Stream[A @uV]
  type jUri             = java.net.URI
  type jUrl             = java.net.URL

  type CopyOption          = jnf.CopyOption
  type DirStreamFilter[A]  = jnf.DirectoryStream.Filter[A]
  type FileStore           = jnf.FileStore
  type FileSystem          = jnf.FileSystem
  type FileSystemProvider  = jnf.spi.FileSystemProvider
  type FileVisitOption     = jnf.FileVisitOption
  type FileVisitor[A]      = jnf.FileVisitor[A]
  type LinkOption          = jnf.LinkOption
  type OpenOption          = jnf.OpenOption
  type Path                = jnf.Path
  type PathDirStream       = jnf.DirectoryStream[Path]
  type NoSuchFileException = jnf.NoSuchFileException

  type AnyFileAttr         = jnfa.FileAttribute[_]
  type BasicFileAttributes = jnfa.BasicFileAttributes
  type FileAttributeView   = jnfa.FileAttributeView
  type FileAttribute[A]    = jnfa.FileAttribute[A]
  type FileTime            = jnfa.FileTime
  type GroupPrincipal      = jnfa.GroupPrincipal
  type PosixFileAttributes = jnfa.PosixFileAttributes
  type PosixFilePermission = jnfa.PosixFilePermission
  type UserPrincipal       = jnfa.UserPrincipal

  type BufferedInputStream  = java.io.BufferedInputStream
  type BufferedReader       = java.io.BufferedReader
  type BufferedWriter       = java.io.BufferedWriter
  type ByteArrayInputStream = java.io.ByteArrayInputStream
  type ByteBuffer           = java.nio.ByteBuffer
  type Charset              = java.nio.charset.Charset
  type File                 = java.io.File
  type FileChannel          = jnc.FileChannel
  type FileInputStream      = java.io.FileInputStream
  type FileOutputStream     = java.io.FileOutputStream
  type IOException          = java.io.IOException
  type InputStream          = java.io.InputStream
  type OutputStream         = java.io.OutputStream
  type SeekableByteChannel  = jnc.SeekableByteChannel
}
