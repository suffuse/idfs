import scala.util.{ Success, Failure }
import scala.sys.process.{ Process, ProcessIO, BasicIO }
import java.io.{ InputStream, OutputStream }
import sfs.api._

package object sfs {

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

  def exec(argv: String*): ExecResult =
    exec(None, argv)

  def exec(input: Array[Byte], argv: String *): ExecResult =
    exec(Some(input), argv)

  def exec(input: Option[Array[Byte]], argv: Seq[String]): ExecResult = {
    val cmd      = argv.toVector
    var out, err = Vector[Byte]()

    def sendInput: OutputStream => Unit = { out => input foreach out.write; out.close() }
    def connect(out: OutputStream): InputStream => Unit = BasicIO.transferFully(_, out)
    def process(f: Byte => Unit) = connect(new OutputStream { def write(b: Int) = f(b.toByte) })

    val io    = new ProcessIO(sendInput, process(out :+= _), process(err :+= _))
    val exit  = Process(cmd, None).run(io).exitValue()

    ExecResult(cmd, exit, out.toArray, err.toArray)
  }

  implicit class AnyOps[A](val x: A) {
    def id_## : Int            = java.lang.System.identityHashCode(x)
    def id_==(y: Any): Boolean = x.asInstanceOf[AnyRef] eq y.asInstanceOf[AnyRef]  // Calling eq on Anys.

    @inline def |>[B](f: A => B): B = f(x) // The famed forward pipe.
  }

  implicit class TryOps[A](x: Try[A]) {
    def |(alt: => A): A                          = x getOrElse alt
    def ||(alt: => Try[A]): Try[A]               = x orElse alt
    def fold[B](l: Throwable => B, r: A => B): B = x match {
      case Success(x) => r(x)
      case Failure(t) => l(t)
    }
  }

  implicit class ClassOps[A](val c: Class[A]) {
    def shortName: String = ((c.getName.stripSuffix("$") split "[.]").last split "[$]").last
  }

  implicit class StringOps(val s: String) extends AnyVal {
    def extension = {
      val i = s.lastIndexOf(".")
      if (i < 0) ""
      else s.substring(i + 1)
    }

    def replaceExtension(newExtenstion: String) =
      s.stripSuffix(extension) + newExtenstion
  }
}
