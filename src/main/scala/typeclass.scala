package suffuse
package api

trait Empty[+A] {
  def emptyValue: A
}

object Empty {
  def apply[A](value: A): Empty[A]        = new Empty[A] { def emptyValue = value }
  def apply[A]()(implicit z: Empty[A]): A = z.emptyValue
}
