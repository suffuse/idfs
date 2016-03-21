package sfs
package api

import java.nio.file.{ attribute => jnfa }
import java.util.concurrent.TimeUnit

trait Api {
  type =?>[-A, +B] = scala.PartialFunction[A, B]
  type Buf         = java.nio.ByteBuffer
  type CTag[A]     = scala.reflect.ClassTag[A]
  type Duration    = scala.concurrent.duration.Duration
  type FileTime    = java.nio.file.attribute.FileTime
  type Iso[A]      = A => A
  type Try[+A]     = scala.util.Try[A]
  type uV          = scala.annotation.unchecked.uncheckedVariance

  def unit = ()

  def classOf[A](implicit z: CTag[A]): Class[A] = classTag[A].runtimeClass.asInstanceOf[Class[A]]
  def classTag[A](implicit z: CTag[A]): CTag[A] = z

  def andTrue(x: Unit): Boolean         = true
  def doto[A](x: A)(f: A => Unit): A    = { f(x) ; x }
  def effect[A](x: A)(effects: Any*): A = x
  def empty[A](implicit z: Empty[A]): A = z.emptyValue
  def Try[A](body: => A): Try[A]        = scala.util.Try[A](body)

  def fileTimeInMillis(ms: Long): FileTime    = jnfa.FileTime fromMillis ms
  def fileTimeInNanos(nanos: Long): FileTime  = jnfa.FileTime.from(nanos, TimeUnit.NANOSECONDS)
  def fileTimeInSeconds(secs: Long): FileTime = jnfa.FileTime.from(secs, TimeUnit.SECONDS)
}
