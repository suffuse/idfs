package sfs
package api

import scala.concurrent.duration.Duration

trait Empty[+A] {
  def emptyValue: A
}
object Empty {
  def const[A](value: A): Empty[A]    = new Empty[A] { def emptyValue = value }
  def apply[A](value: => A): Empty[A] = new Empty[A] { def emptyValue = value }
}

trait Functor[F[_]] {
  def map[A, B](f: A => B): F[A] => F[B]
}

trait TimeStamp[A] {
  def add(value: A)(amount: Duration): A
}

trait ShowDirect extends Any {
  def to_s: String
}

trait ShowSelf extends Any with ShowDirect {
  override def toString = to_s
}
