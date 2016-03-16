package suffuse
package api

trait Empty[+A] {
  def emptyValue: A
}

object Empty {
  def apply[A](value: A): Empty[A] = new Empty[A] { def emptyValue = value }
}
