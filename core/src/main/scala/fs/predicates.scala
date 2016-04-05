package sfs
package fs

import api._

sealed trait Predicate[A] extends (A => Boolean) {
  def apply(a: A): Boolean
}
case class Not[A](predicate: Predicate[A]) extends Predicate[A] {
  def apply(a: A): Boolean = !predicate(a)
}
case class RegexPredicate(regex: String) extends Predicate[Path] {
  def apply(path: Path) = path.to_s matches regex
}
