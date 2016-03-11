package suffuse

import java.nio.{ file => jnf }
import java.nio.{ channels => jnc }
import jnf.{ attribute => jnfa }
import jnf.{ Files }
import scala.collection.JavaConverters._

package object jio extends JioFiles {
  val UTF8 = java.nio.charset.Charset forName "UTF-8"

  implicit class FileOps(val f: File) extends AnyVal {
    def mtime: Long      = f.lastModified / 1000L
    def blockCount: Long = (f.length + blockSize - 1) / blockSize
    def blockSize: Int   = 512 // FIXME

    def appending[A](g: FileOutputStream => A): A = {
      val stream = new FileOutputStream(f, true) // append = true
      try g(stream) finally stream.close()
    }
  }

  implicit class StreamOps[A](val xs: jStream[A]) extends AnyVal {
    def toVector: Vector[A] = {
      val buf = Vector.newBuilder[A]
      xs.iterator.asScala foreach (x => buf += x)
      buf.result
    }
  }

  implicit class PathOps(val p: Path) extends AnyVal {
    def /(name: String): Path = p resolve name

    def mkdir(attrs: AnyFileAttr*): Path = createDirectory(p, attrs: _*)
    def exists                           = p.toFile.exists
    def isSymbolicLink()                 = Files isSymbolicLink p
    def readSymbolicLink()               = Files readSymbolicLink p
    def readAllBytes()                   = Files readAllBytes p
    def list(): Vector[Path]             = Files list p toVector
    def filename: String                 = p.getFileName.toString

    def openChannel(opts: OpenOption*): FileChannel = jnc.FileChannel.open(p, opts: _*)
  }

  def file(s: String, ss: String*): File = ss.foldLeft(new File(s))(new File(_, _))
  def path(s: String, ss: String*): Path = ss.foldLeft(jnf.Paths get s)(_ resolve _)
  def homeDir: Path                      = path(sys.props("user.home"))

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
