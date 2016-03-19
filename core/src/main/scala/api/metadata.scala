package sfs
package api

object Example {
  final case class Mtime(timestamp: Long)
  final case class Atime(timestamp: Long)
  final case class Size(bytes: Long)

  // Using these explicitly should be discouraged, this can be a convention rather than an enforcement
  implicit val mtime = new Key[Mtime]("modification time")
  implicit val atime = new Key[Atime]("access time")
  implicit val size  = new Key[Size]("size in bytes")

  implicit def emptySize: Empty[Size] = Empty(Size(-1L))

  def main(args: Array[String]): Unit = {
    var attrs = Metadata set Mtime(123L)
    println("Size is " + attrs[Size])
    attrs = attrs set Size(456L)
    println(attrs)
    attrs = attrs set Size(10000L) set Atime(789)
    println(attrs)
    println("Size is " + attrs[Size])
  }
}

/*
 * Conventions:
 *
 * - methods that are intended to be used with 'type parameters only' should be written:
 *     `def name[A]()(implicit z: Implicit[A]): ReturnType`
 *   The argument list should be empty and the implicit should be in the second argument list.
 *   This makes explicit passing of the argument awkward. Implicit arguments are typically called `z`
 * - limit the use of `val` to an absolute minimum
 * - think twice before writing the `extends` or `override` keyword.
 */

/** A Metadata map, holding any number of well typed Attributes.
 *  A typed value can be obtained for any key.
 */
sealed class Metadata[+Z](val attributes: Vector[Attribute]) extends ShowSelf { md =>

  private val untypedMap                         = attributes.foldLeft(Map[Key[_], Any]())(_ + _.pair)
  private def untypedAs[A]()(implicit z: Key[A]) = untypedMap(z).asInstanceOf[A]

  def apply[A: Empty]()(implicit z: Key[A]): A = fold(ifValue = identity[A], orElse = empty[A])

  def isEmpty                                  = untypedMap.isEmpty
  def has[A: Key](a: A):Boolean                = fold[A](ifValue = _ == a, orElse = false)
  def has[A]()(implicit z: Key[A]): Boolean    = attributes exists (_ hasKey z)
  def keys: Vector[Key[_]]                     = attributes map (_.key)

  def drop(attr: Attribute): Metadata[Z]          = drop()(attr.key)
  def drop[A]()(implicit z: Key[A]): Metadata[Z]  = if (has[A]) mapAttributes(_ filterNot (_ hasKey z)) else this

  /** Set could also hold onto the old value, and just return the last one.
   *  It would amount to preserving the attribute's history.
   */
  def set(attr: Attribute): Metadata[Z]  = drop(attr) mapAttributes (_ :+ attr)
  def set[A: Key](value: A): Metadata[Z] = set(Attribute[A](value))

  def fold[A]: Fold[A] = new Fold[A]
  class Fold[A] {
    def apply[B](ifValue: A => B, orElse: => B)(implicit z: Key[A]): B =
      if (has[A]) ifValue(untypedAs[A]) else orElse
  }

  def foreach[A: Key](f: A => Unit): Unit = fold[A](f, unit)

  def mapAttributes(f: Vector[Attribute] => Vector[Attribute]): Metadata[Z] = new api.Metadata[Z](f(attributes))

  def to_s = if (isEmpty) "{ }" else attributes mkString ("{\n  ", "\n  ", "\n}")
}
object Metadata extends Metadata(Vector()) {
  def apply[Z](attributes: Attribute*): Metadata[Z] = attributes.foldLeft(this: Metadata[Z])(_ set _)
}

/** Attribute is a dependently typed key/value pair.
 */
trait Attribute extends Any with ShowDirect {
  type Type
  def key: Key[Type]
  def value: Type

  def hasSameKey(that: Attribute)      = this hasKey that.key
  def hasKey[A](implicit that: Key[A]) = key == that

  def pair = key -> value
  def to_s = s"$key: $value"
}

object Attribute {
  type Of[A] = Attribute { type Type = A }

  def apply[A](value: A)(implicit key: Key[A]): Of[A]    = AttributeOf[A](key, value)
  def unapply(x: Attribute): Some[(Key[x.Type], x.Type)] = Some(x.pair)

  /** The simple implementation of Attribute, which ties the knot between the
   *  user-facing type parameter and the type member which couples key and value.
   */
  private case class AttributeOf[A](key: Key[A], value: A) extends Attribute with ShowSelf { type Type = A }
}

/** Key is an attribute key: a Metadata map key which carries
 *  both the type of the corresponding value, and a value to
 *  provide when there is no value in the map. This as opposed
 *  to wrapping everything in sight in Option.
 */
final class Key[A](description: String) extends ShowSelf {
  def to_s = description
}
