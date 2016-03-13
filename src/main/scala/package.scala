import java.nio.file._
import net.fusejna.ErrorCodes._
import scala.util.{ Success, Failure }
import scala.sys.process.{ Process, ProcessLogger }

package object suffuse {
  type uV                = scala.annotation.unchecked.uncheckedVariance
  type FuseException     = net.fusejna.FuseException
  type StructFuseContext = net.fusejna.StructFuseContext
  type Try[+A]           = scala.util.Try[A]

  def Try[A](body: => A): Try[A]        = scala.util.Try[A](body)
  def alreadyExists()                   = -EEXIST
  def doesNotExist()                    = -ENOENT
  def effect[A](x: A)(effects: Any*): A = x
  def isNotValid()                      = -EINVAL
  def notImplemented()                  = -ENOSYS
  def notSupported()                    = notImplemented()
  def eok()                             = 0
  def tryFuse(body: => Unit): Int       = Try(body) fold (_.toErrno, _ => eok)

  def exec(argv: String*): ExecResult = {
    val cmd      = argv.toVector
    var out, err = Vector[String]()
    val logger   = ProcessLogger(out :+= _, err :+= _)
    val exit     = Process(cmd, None) ! logger

    ExecResult(cmd, exit, out, err)
  }

  implicit class ThrowableOps(t: Throwable) {
    println(t)
    def toErrno: Int = t match {
      case _: FileAlreadyExistsException    => alreadyExists()
      case _: NoSuchFileException           => doesNotExist()
      case _: IllegalArgumentException      => isNotValid()
      case _: UnsupportedOperationException => notImplemented()
      case _: AccessDeniedException         => -EACCES
      case _: jio.IOException               => -EIO
      case _                                => -EIO
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
