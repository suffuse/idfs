import scala.util.{ Success, Failure }
import scala.sys.process.{ Process, ProcessLogger }
import sfs.api._

package object sfs extends sfs.api.Api {

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
    var out, err = Vector[String]()
    val logger   = ProcessLogger(out :+= _, err :+= _)
    val exit     = Process(cmd, None) ! logger

    ExecResult(cmd, exit, out, err)
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

  trait Functor[F[_]] {
    def map[A, B](f: A => B): F[A] => F[B]
  }
  object Functor {
    implicit def forMappable[A, M[A] <: { def map[B](f: A => B): M[B] }]: Functor[M] =
      new Functor[M] { def map[A, B](f: A => B): M[A] => M[B] = _ map f }
  }
  implicit class FunctorOps[F[_], A](fa: F[A])(implicit F: Functor[F]) {
    def map[B](f: A => B): F[B] = F.map(f)(fa)
  }
}
