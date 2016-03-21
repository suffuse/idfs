package sfs
package api

trait Empty[+A] {
  def emptyValue: A
}

object Empty {
  def apply[A](value: A): Empty[A] = new Empty[A] { def emptyValue = value }
}

trait Functor[F[_]] {
  def map[A, B](f: A => B): F[A] => F[B]
}
