package sfs
package api

import scala.language.higherKinds

trait Empty[+A] {
  def emptyValue: A
}
object Empty {
  def const[A](value: A): Empty[A]    = new Empty[A] { def emptyValue = value }
  def apply[A](value: => A): Empty[A] = new Empty[A] { def emptyValue = value }
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

trait ~>[-F[_], +G[_]] { fToG =>
  def apply[A](fa: F[A]): G[A]

  def andThen[H[_]](gToH: G ~> H): F ~> H =
    new (F ~> H) { def apply[A](fa: F[A]): H[A] = gToH(fToG(fa)) }
}
