package suffuse

import api._

/** AKey is an attribute key: a Metadata map key which carries
 *  both the type of the corresponding value, and a value to
 *  provide when there is no value in the map. This as opposed
 *  to wrapping everything in sight in Option.
 */
class AKey[A](description: String) extends ShowSelf {
  def to_s = description
}

/** KVPair is a dependently typed key/value pair.
 */
trait KVPair extends Any with ShowDirect {
  type Type
  def key: AKey[Type]
  def value: Type

  def hasSameKey(that: KVPair)          = this hasKey that.key
  def hasKey[A](implicit that: AKey[A]) = key == that

  def pair = key -> value
  def to_s = s"$key: $value"
}

object KVPair {
  type Of[A] = KVPair { type Type = A }

  /** The simple implementation of KVPair, which ties the knot between the
   *  user-facing type parameter and the type member which couples key and value.
   */
  private case class KVPairOf[A](key: AKey[A], value: A) extends KVPair with ShowSelf { type Type = A }

  def apply[A](value: A)(implicit key: AKey[A]): Of[A] = new KVPairOf[A](key, value)
  def unapply(x: KVPair): Some[(AKey[x.Type], x.Type)] = Some(x.pair)
}

/** A Metadata map, holding any number of well typed KVPairs.
 *  A typed value can be obtained for any key.
 */
sealed class Metadata(pairs: Vector[KVPair]) extends ShowSelf {
  private[this] val untypedMap                                        = pairs.foldLeft(Map[AKey[_], Any]())(_ + _.pair)
  private def doApply[A]()(implicit z: AKey[A]): A                    = untypedMap(z).asInstanceOf[A]
  private def mapPairs(f: Vector[KVPair] => Vector[KVPair]): Metadata = new Metadata(f(pairs))

  def isEmpty                                          = untypedMap.isEmpty
  def apply[A]()(implicit z: AKey[A], ze: Empty[A]): A = if (has[A]) doApply[A]() else ze.emptyValue
  def has[A]()(implicit z: AKey[A]): Boolean           = pairs exists (_ hasKey z)
  def keys: Vector[AKey[_]]                            = pairs map (_.key)
  def drop[A]()(implicit z: AKey[A]): Metadata         = if (has[A]) mapPairs(_ filterNot (_ hasKey z)) else this

  /** Set could also hold onto the old value, and just return the last one.
   *  It would amount to preserving the attribute's history.
   */
  def set(attr: KVPair): Metadata      = drop()(attr.key) mapPairs (_ :+ attr)
  def set[A: AKey](value: A): Metadata = set(KVPair[A](value))

  def to_s = if (isEmpty) "{ }" else pairs mkString ("{\n  ", "\n  ", "\n}")
}
object Metadata extends Metadata(Vector()) {
  def apply(pairs: KVPair*): Metadata = pairs.foldLeft(this: Metadata)(_ set _)
}

object Example {
  final case class Mtime(timestamp: Long)
  final case class Atime(timestamp: Long)
  final case class Size(bytes: Long)

  implicit object Mtime extends AKey[Mtime]("modification time")
  implicit object Atime extends AKey[Atime]("access time")
  implicit object Size extends AKey[Size]("size in bytes")

  implicit def emptySize: Empty[Size] = Empty(Size(-1L))

  def main(args: Array[String]): Unit = {
    var attrs = Metadata set Mtime(123L) set Size(456L)
    println(attrs)
    attrs = attrs set Size(10000L) set Atime(789)
    println(attrs)
    println("Size is " + attrs[Size])
  }
}
