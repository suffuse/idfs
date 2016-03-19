package sfs

import java.nio.ByteBuffer
import java.nio.{ file => jnf }
import java.nio.{ channels => jnc }
import jnf.{ attribute => jnfa }
import jnf.{ Files }
import jnf.LinkOption.NOFOLLOW_LINKS
import jnfa.PosixFilePermissions.asFileAttribute
import java.util.concurrent.TimeUnit
import javax.naming.SizeLimitExceededException
import scala.collection.convert.{ DecorateAsScala, DecorateAsJava }
import api._

package object jio extends JioFiles with DecorateAsScala with DecorateAsJava {
  val UTF8 = java.nio.charset.Charset forName "UTF-8"

  def jList[A](xs: A*): jList[A] = doto(new java.util.ArrayList[A])(xs foreach _.add)

  implicit class StreamOps[A](val xs: jStream[A]) extends AnyVal {
    def toVector: Vector[A] = {
      val buf = Vector.newBuilder[A]
      xs.iterator.asScala foreach (x => buf += x)
      buf.result
    }
  }

//  implicit class FileOps(val f: File) extends Pathish[File] {
//    def path: Path     = f.toPath
//    def asRep(p: Path) = p.toFile
//
//    def appending[A](g: FileOutputStream => A): A = {
//      val stream = new FileOutputStream(f, true) // append = true
//      try g(stream) finally stream.close()
//    }
//  }
  implicit class PathOps(val path: Path) extends Pathish[Path] {
    def asRep(p: Path) = p

    def metadata: Metadata[Java] = ???

    def permissions: Set[jnfa.PosixFilePermission] =
      (Files getPosixFilePermissions (path, NOFOLLOW_LINKS)).asScala.toSet

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

  // marker interface
  sealed trait Java

  trait Pathish[Rep] {
    def path: Path
    def asRep(p: Path): Rep

    def /(name: String): Rep      = asRep(path resolve name)
    def mkdir(metadata: Metadata[Java]): Rep    = asRep(createDirectory(path, asFileAttribute(metadataAsPermissions(metadata))))
    def mkfile(metadata: Metadata[Java]): Rep   = asRep(createFile(path, asFileAttribute(metadataAsPermissions(metadata))))
    def mklink(target: Path): Rep = asRep(createSymbolicLink(path, target))

    private def withDirStream[A](dir: Path)(code: jStream[Path] => A): A =
      (Files list dir) |> (str => try code(str) finally str.close())

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
    def depth: Int                      = path.getNameCount
    def exists: Boolean                 = Files.exists(path, NOFOLLOW_LINKS)
    def filename: String                = path.getFileName.to_s
    def isDirectory: Boolean            = Files.isDirectory(path, NOFOLLOW_LINKS)
    def isFile: Boolean                 = Files.isRegularFile(path, NOFOLLOW_LINKS)
    def isSymbolicLink: Boolean         = Files isSymbolicLink path
    def ls: Vector[Rep]                 = if (isDirectory) withDirStream(path)(_.toVector map asRep) else Vector()
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

  //def file(s: String, ss: String*): File   = ss.foldLeft(new File(s))(new File(_, _))
  def toPath(s: String, ss: String*): Path = ss.foldLeft(jnf.Paths get s)(_ resolve _)
  def homeDir: Path                        = toPath(sys.props("user.home"))

  def metadataAsPermissions(metadata: Metadata[Java]) = ???
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

  final class NodeType(`type`: String) extends api.ShowSelf {

    def asJava = ???

    def to_s = `type`
  }
  object NodeType {

    def fromJava: Nothing => NodeType = ???
  }

  val File = new NodeType("file")
  val Dir  = new NodeType("dir" )
  val Link = new NodeType("link")
  implicit val _nodeType = new api.Key[NodeType]("type of node")

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
  //type File                 = java.io.File
  type FileChannel          = jnc.FileChannel
  type FileInputStream      = java.io.FileInputStream
  type FileOutputStream     = java.io.FileOutputStream
  type IOException          = java.io.IOException
  type InputStream          = java.io.InputStream
  type OutputStream         = java.io.OutputStream
  type SeekableByteChannel  = jnc.SeekableByteChannel
}
