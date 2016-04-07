package sfs
package fs

import api._

sealed trait Predicate[A] /* do not extens (A => Boolean) */ {
  def asFunction: A => Boolean
}
case class Not[A](predicate: Predicate[A]) extends Predicate[A] {
  def asFunction = !predicate.asFunction(_)
}
case class RegexPredicate(regex: String) extends Predicate[Path] {
  def asFunction = _.to_s matches regex
}
