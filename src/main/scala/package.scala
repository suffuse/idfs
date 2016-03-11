import java.nio.file._
import java.nio.channels.FileChannel
import net.fusejna.ErrorCodes._
import scala.util.{ Try, Success, Failure }

package object psp {
  val UTF8 = java.nio.charset.Charset forName "UTF-8"

  type ByteBuffer    = java.nio.ByteBuffer
  type Path          = java.nio.file.Path
  type File          = java.io.File
  type IOException   = java.io.IOException
  type FuseException = net.fusejna.FuseException

  def Try[A](body: => A): Try[A]         = scala.util.Try[A](body)
  def file(s: String, ss: String*): File = ss.foldLeft(new File(s))(new File(_, _))
  def path(s: String, ss: String*): Path = ss.foldLeft(Paths get s)(_ resolve _)
  def also[A](x: A)(effects: Any*): A    = x

  def tryFuse(body: => Unit): Int = Try(body) match {
    case Failure(t) => t.toErrno
    case Success(_) => EOK
  }

  def EOK             = 0
  def alreadyExists() = -EEXIST
  def doesNotExist()  = -ENOENT
  def isNotValid()    = -EINVAL

  implicit class ThrowableOps(t: Throwable) {
    def toErrno: Int = t match {
      case _: NotDirectoryException         => ENOTDIR
      case _: DirectoryNotEmptyException    => ENOTEMPTY
      case _: FileAlreadyExistsException    => EEXIST
      case _: NoSuchFileException           => ENOENT
      case _: NotLinkException              => EPERM
      case _: IllegalArgumentException      => EINVAL
      case _: UnsupportedOperationException => ENOTSUP
      case _: SecurityException             => EPERM
      case _: IOException                   => EIO
      case _                                => EIO
    }
  }

  implicit class TryOps[A](x: Try[A]) {
    def |(alt: => A): A = x.toOption getOrElse alt

    def ||(alt: => Try[A]): Try[A] = x match {
      case Failure(_) => alt
      case _          => x
    }
  }

  implicit class PathOps(p: Path) {
    def exists             = p.toFile.exists
    def isSymbolicLink()   = Files isSymbolicLink p
    def readSymbolicLink() = Files readSymbolicLink p
    def readAllBytes()     = Files readAllBytes p

    def openChannel(opts: OpenOption*): FileChannel = FileChannel.open(p, opts: _*)
  }
}
