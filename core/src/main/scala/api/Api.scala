package sfs
package api

import java.util.concurrent.TimeUnit

trait Api {
  type =?>[-A, +B] = scala.PartialFunction[A, B]
  type Attribute   = metadata.Attribute
  val  Attribute   = metadata.Attribute
  type Buf         = java.nio.ByteBuffer
  type CTag[A]     = scala.reflect.ClassTag[A]
  type Data        = Array[Byte]
  type Duration    = scala.concurrent.duration.Duration
  type FileTime    = java.nio.file.attribute.FileTime
  type Iso[A]      = A => A
  type Key[A]      = metadata.KeyDefinition[A]
  type LinkTarget  = String // Could be anything as long as implementations on both sides are able to serialize to it
  type Metadata    = metadata.Metadata
  val  Metadata    = metadata.Metadata
  type Name        = String
  type Path        = jio.Path
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
}
