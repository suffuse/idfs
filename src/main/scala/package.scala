import java.nio.file._
import net.fusejna.ErrorCodes._
import scala.util.{ Try, Success, Failure }

package object suffuse {
  type uV                = scala.annotation.unchecked.uncheckedVariance
  type FuseException     = net.fusejna.FuseException
  type StructFuseContext = net.fusejna.StructFuseContext

  def Try[A](body: => A): Try[A]        = scala.util.Try[A](body)
  def alreadyExists()                   = -EEXIST
  def doesNotExist()                    = -ENOENT
  def effect[A](x: A)(effects: Any*): A = x
  def isNotValid()                      = -EINVAL
  def notImplemented()                  = -ENOSYS
  def eok()                             = 0
  def tryFuse(body: => Unit): Int       = Try(body) fold (_.toErrno, _ => eok)

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
      case _: jio.IOException               => EIO
      case _                                => EIO
    }
  }
  implicit class TryOps[A](x: Try[A]) {
    def |(alt: => A): A                          = x getOrElse alt
    def ||(alt: => Try[A]): Try[A]               = x orElse alt
    def fold[B](l: Throwable => B, r: A => B): B = x match {
      case Success(x) => r(x)
      case Failure(t) => l(t)
    }
  }
}
