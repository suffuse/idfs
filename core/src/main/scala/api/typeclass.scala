package sfs
package api

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
