import java.nio.file._
import javax.naming.SizeLimitExceededException
import net.fusejna.ErrorCodes._
import scala.util.{ Success, Failure }
import scala.sys.process.{ Process, ProcessIO, BasicIO }
import java.io.InputStream
import java.io.OutputStream

package object suffuse {
  type Buf     = java.nio.ByteBuffer
  type Try[+A] = scala.util.Try[A]
  type uV      = scala.annotation.unchecked.uncheckedVariance

  def Try[A](body: => A): Try[A]        = scala.util.Try[A](body)
  def tryFuse(body: => Unit): Int       = Try(body) fold (_.toErrno, _ => eok)
  def andTrue(x: Unit): Boolean         = true
  def doto[A](x: A)(f: A => Unit): A    = { f(x) ; x }
  def effect[A](x: A)(effects: Any*): A = x

  def alreadyExists()  = -EEXIST
  def doesNotExist()   = -ENOENT
  def eok()            = 0
  def isMac            = scala.util.Properties.isMac
  def isNotValid()     = -EINVAL
  def notImplemented() = -ENOSYS
  def notSupported()   = notImplemented()

  // For example statsBy(path("/usr/bin").ls)(_.mediaType.subtype)
  //
  // 675   octet-stream
  // 96    x-shellscript
  // 89    x-perl
  // 26    x-c
  // ...
  def statsBy[A, B](xs: Seq[A])(f: A => B): Unit = {
    val counts = xs groupBy f mapValues (_.size)
    counts.toVector sortBy (-_._2) map { case (k, n) => "%-5s %s".format(n, k) } foreach println
  }

  def exec(argv: String*): ExecResult = {
    val cmd      = argv.toVector
    var out, err = Vector[Byte]()

    import BasicIO._
    def connect(out: OutputStream): InputStream => Unit = transferFully(_, out)
    def process(f: Byte => Unit) = connect(new OutputStream { def write(b: Int) = f(b.toByte) })

    val io    = new ProcessIO(input(false), process(out :+= _), process(err :+= _))
    val exit  = Process(cmd, None).run(io).exitValue()

    ExecResult(cmd, exit, out.toArray, err.toArray)
  }

  implicit class ThrowableOps(t: Throwable) {
    println(t)
    def toErrno: Int = t match {
      case _: FileAlreadyExistsException    => alreadyExists()
      case _: NoSuchFileException           => doesNotExist()
      case _: IllegalArgumentException      => isNotValid()
      case _: UnsupportedOperationException => notImplemented()
      case _: DirectoryNotEmptyException    => -ENOTEMPTY
      case _: SizeLimitExceededException    => -EFBIG
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
