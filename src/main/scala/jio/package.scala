package suffuse

import java.nio.{ file => jnf }
import java.nio.{ channels => jnc }
import jnf.{ attribute => jnfa }
import jnf.{ Files }
import jnf.LinkOption.NOFOLLOW_LINKS
import jnfa.PosixFilePermissions.asFileAttribute
import scala.collection.JavaConverters._
import java.util.concurrent.TimeUnit
import java.nio.ByteBuffer

package object jio extends JioFiles {
  val UTF8 = java.nio.charset.Charset forName "UTF-8"


  implicit class StreamOps[A](val xs: jStream[A]) extends AnyVal {
    def toVector: Vector[A] = {
      val buf = Vector.newBuilder[A]
      xs.iterator.asScala foreach (x => buf += x)
      buf.result
    }
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
      case c if c.size < size => c write (ByteBuffer wrap Array.fill((size - c.size).toInt)(0.toByte), c.size)
      case _                  => // sizes are equal
    }

    private def withWriteChannel[A](code: jnc.FileChannel => A): A = {
      val channel = jnc.FileChannel.open(path, jnf.StandardOpenOption.WRITE)
      try code(channel)
      finally channel.close()
    }
  }

  trait Pathish[Rep] {
    def path: Path
    def asRep(p: Path): Rep

    def /(name: String): Rep      = asRep(path resolve name)
    def mkdir(bits: Long): Rep    = asRep(createDirectory(path, asFileAttribute(bitsAsPermissions(bits))))
    def mkfile(bits: Long): Rep   = asRep(createFile(path, asFileAttribute(bitsAsPermissions(bits))))
    def mklink(target: Path): Rep = asRep(createSymbolicLink(path, target))

    /** Some consistent naming scheme for various operations would be a boon.
     *    isdir isfile islink?
     *    readdir readfile readlink?
     */

    def allBytes: Array[Byte]           = Files readAllBytes path
    def atime: Long                     = attributes.lastAccessTime.toMillis
    def attributes: BasicFileAttributes = Files readAttributes(path, classOf[BasicFileAttributes], NOFOLLOW_LINKS)
    def blockCount: Long                = (attributes.size + blockSize - 1) / blockSize
    def blockSize: Long                 = 512 // FIXME
    def delete(): Unit                  = Files delete path
    def exists: Boolean                 = Files.exists(path, NOFOLLOW_LINKS)
    def filename: String                = path.getFileName.to_s
    def isDirectory: Boolean            = Files.isDirectory(path, NOFOLLOW_LINKS)
    def isFile: Boolean                 = Files.isRegularFile(path, NOFOLLOW_LINKS)
    def isSymbolicLink: Boolean         = Files isSymbolicLink path
    def ls: Vector[Rep]                 = (Files list path).toVector map asRep
    def mediaType: MediaType            = MediaType(exec("file", "--brief", "--mime", "--dereference", to_s).stdout mkString "\n")
    def mtime: Long                     = attributes.lastModifiedTime.toMillis
    def readlink: Rep                   = asRep(Files readSymbolicLink path)
    def size: Long                      = attributes.size
    def to_s: String                    = path.toString
    def moveTo(target: Path)            = Files.move(path, target)
  }

  case class PosixFilePermissions(
    groupRead: Boolean, groupWrite: Boolean, groupExecute: Boolean,
    ownerRead: Boolean, ownerWrite: Boolean, ownerExecute: Boolean,
    otherRead: Boolean, otherWrite: Boolean, otherExecute: Boolean
  )

  def file(s: String, ss: String*): File = ss.foldLeft(new File(s))(new File(_, _))
  def path(s: String, ss: String*): Path = ss.foldLeft(jnf.Paths get s)(_ resolve _)
  def homeDir: Path                      = path(sys.props("user.home"))

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

  type CopyOption         = jnf.CopyOption
  type DirStreamFilter[A] = jnf.DirectoryStream.Filter[A]
  type FileStore          = jnf.FileStore
  type FileSystem         = jnf.FileSystem
  type FileSystemProvider = jnf.spi.FileSystemProvider
  type FileVisitOption    = jnf.FileVisitOption
  type FileVisitor[A]     = jnf.FileVisitor[A]
  type LinkOption         = jnf.LinkOption
  type OpenOption         = jnf.OpenOption
  type Path               = jnf.Path
  type PathDirStream      = jnf.DirectoryStream[Path]

  type BasicFileAttributes  = jnfa.BasicFileAttributes
  type AnyFileAttr          = jnfa.FileAttribute[_]
  type FileAttributeView    = jnfa.FileAttributeView
  type FileAttribute[A]     = jnfa.FileAttribute[A]
  type FileTime             = jnfa.FileTime
  type PosixFilePermission  = jnfa.PosixFilePermission
  type UserPrincipal        = jnfa.UserPrincipal

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
