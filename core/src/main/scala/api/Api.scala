package sfs
package api

trait Api {
  type Buf         = java.nio.ByteBuffer
  type Try[+A]     = scala.util.Try[A]
  type uV          = scala.annotation.unchecked.uncheckedVariance
  type =?>[-A, +B] = scala.PartialFunction[A, B]
  type Iso[A]      = A => A
  type CTag[A]     = scala.reflect.ClassTag[A]

  def classOf[A](implicit z: CTag[A]): Class[A] = classTag[A].runtimeClass.asInstanceOf[Class[A]]
  def classTag[A](implicit z: CTag[A]): CTag[A] = z

  def andTrue(x: Unit): Boolean         = true
  def doto[A](x: A)(f: A => Unit): A    = { f(x) ; x }
  def effect[A](x: A)(effects: Any*): A = x
  def empty[A](implicit z: Empty[A]): A = z.emptyValue
  def Try[A](body: => A): Try[A]        = scala.util.Try[A](body)
}
