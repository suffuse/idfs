package suffuse

/** AKey is an attribute key: a Metadata map key which carries
 *  both the type of the corresponding value, and a value to
 *  provide when there is no value in the map. This as opposed
 *  to wrapping everything in sight in Option.
 */
final class AKey[A](description: String) {
  override def toString = description
}

/** Attr is a dependently typed key/value pair.
 */
trait Attr extends Any {
  type Type
  def key: AKey[Type]
  def value: Type

  def pair                               = key -> value
  def matches[A](implicit that: AKey[A]) = key == that
  def to_s                               = s"$key: $value"
}

/** The simple implementation of Attr, which ties the knot between the
 *  user-facing type parameter and the type member which couples key and value.
 */
final case class AttrOf[A](key: AKey[A], value: A) extends Attr {
  type Type = A
  override def toString = s"$key: $value"
}

object Attr {
  def apply[A](value: A)(implicit key: AKey[A]): AttrOf[A] = new AttrOf[A](key, value)
  def unapply(x: Attr): Some[(AKey[x.Type], x.Type)]       = Some(x.pair)
}

/** A Metadata map, holding any number of well typed Attrs.
 *  A typed value can be obtained for any key.
 */
sealed class Metadata(attrs: Vector[Attr]) {
  private[this] val untypedMap                             = attrs.foldLeft(Map.empty[AKey[_], Any])(_ + _.pair)
  private def doApply[A]()(implicit z: AKey[A]): Option[A] = untypedMap get z map (_.asInstanceOf[A])

  def isEmpty                                  = untypedMap.isEmpty
  def apply[A](implicit z: AKey[A]): Option[A] = doApply[A]
  def has[A](implicit z: AKey[A]): Boolean     = keys contains z
  def keys: Vector[AKey[_]]                    = attrs map (_.key)

  /** Set could also hold onto the old value, and just return the last one.
   *  It would amount to preserving the attribute's history.
   */
  def set(attr: Attr): Metadata        = new Metadata(attrs.filterNot(_.key eq attr.key) :+ attr)
  def set[A: AKey](value: A): Metadata = set(Attr[A](value))

  override def toString = if (isEmpty) "{ }" else attrs mkString ("{\n  ", "\n  ", "\n}")
}
object Metadata extends Metadata(Vector()) {
  def apply(attrs: Attr*): Metadata = attrs.foldLeft[Metadata](this)(_ set _)
}

object Example {
  /** Implicit keys are the road to strongly typed and usable both.
   */
  implicit val mtime = new AKey[Mtime]("modification time")
  implicit val atime = new AKey[Atime]("access time")
  implicit val size  = new AKey[Size]("size in bytes")

  final case class Mtime(val timestamp: Long)
  final case class Atime(val timestamp: Long)
  final case class Size(val bytes: Long)

  def main(args: Array[String]): Unit = {
    var attrs = Metadata set Mtime(123L) set Size(456L)
    println(attrs)
    attrs = attrs set Size(10000L) set Atime(789)
    println(attrs)
    println("Size is " + attrs[Size])
  }
}
