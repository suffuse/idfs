package sfs
package api

import scala.annotation.unchecked.uncheckedVariance

package object metadata {

  /** Key is an attribute key: a Metadata map key which carries the type of the corresponding
   *  value
   *
   *  With keys we faced a problem. A simple definition of `Key[A]` only allowed a very strict set
   *  of operations. The semantics of retrieving keys was spot-on, but setting a key was tedious.
   *  An example using the 'simple' definition of key:
   *
   *  	final class Key[A](description: String)
   *
   *    sealed trait SomeProperty
   *    object SomeProperty {
   *      def empty: Empty[SomeProperty] = Empty(NoProperty)
   *    }
   *    case object SomeValue extends SomeProperty
   *    case object NoProperty extends SomeProperty
   *    implicit val _someProperty = new Key[SomeProperty]("some property")
   *
   *  This would allow most operations on `Metadata`
   *
   *    metadata.fold[SomeProperty](ifValue = ..., orElse = ...)
   *    metadata[SomeProperty]
   *
   *  Setting a value would however become problematic and not user friendly:
   *
   *    metadata set[SomeProperty] SomeValue
   *    Metadata(identity[SomeProperty](SomeValue))
   *    metadata.only[SomeProperty] map {
   *      case SomeValue  => identity[SomeProperty](NoProperty)
   *      case NoProperty => identity[SomeProperty](SomeValue)
   *    }
   *
   *  A simple (user friendly) solution would be to define `Key` as being covariant `class Key[-A]`,
   *  this would however allow you to typecheck the following:
   *
   *    metadata[SomeValue.type]
   *
   *  As you can imagine, this would cause problems as the value returned would not necessary be
   *  of type `SomeValue.type`, it could just as well be `NoProperty.type`.
   *
   *  This led us to the following key definitions:
   *
   *    StrictKey[A]      - An instance of this can only be obtained when you supply a key as it was
   *                        defined. In our example it would be available for SomeProperty, but not
   *                        for any subtype of SomeProperty
   *    KeyDefinition[-A] - This is the actual key for which instances are provided by the user. It's
   *                        only available to the user through a type alias.
   *
   *  None of these two definitions are used directly in the metadata package, in this package we use
   *  aliases to keep our brain healty and code compact.
   *
   *  In order to make the API user friendly and code readable we have added a few type aliases:
   *
   *    api.Key[A]              - An alias for KeyDefinition[A], this is what users define and work with.
   *    metadata.Key[A]         - An alias that makes sure we use strict version in most places
   *    metadata.LenientKey[-A] - An alias that clearly states the method can be used with a key defined
   *                              for a super type. This allows a user friendly interface for setting
   *                              attributes.
   *
   *  With this new infrastructure in place the example definition of `SomeProperty` stays intact, but
   *  `set` operations become user friendly and safe at the same time.
   *
   *    metadata set SomeValue
   *    Metadata(SomeValue)
   *    metadata.only[SomeProperty] map {
   *      case SomeValue  => NoProperty
   *      case NoProperty => SomeValue
   *    }
   *    metadata[SomeValue.type] // compile error
   */
  private type Key[A]         = StrictKey[A]
  private type LenientKey[-A] = KeyDefinition[A]
  private type StrictKey[A]   = KeyDefinition[A] { type Type = A }

  private[api] final class KeyDefinition[-A](description: String) extends ShowSelf {
    def to_s = description

    private[metadata] type Type = (A @uncheckedVariance)
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
  sealed class Metadata(val attributes: Vector[Attribute]) extends ShowSelf {
    self =>

    private val untypedMap                         = attributes.foldLeft(Map[Key[_], Any]())(_ + _.pair)
    private def untypedAs[A]()(implicit z: Key[A]) = untypedMap(z).asInstanceOf[A]

    def transformAttributes(f: Vector[Attribute] => Vector[Attribute]): Metadata = new Metadata(f(attributes))

    def apply[A: Empty: Key](): A             = fold[A](ifValue = identity[A], orElse = empty[A])

    def isEmpty                               = untypedMap.isEmpty
    def has[A]()(implicit z: Key[A]): Boolean = attributes exists (_ hasKey z)
    def keys: Vector[Key[_]]                  = attributes map (_.key)

    /** Set could also hold onto the old value, and just return the last one.
     *  It would amount to preserving the attribute's history.
     */
    def set[A: LenientKey](value: A): Metadata  = drop[A] transformAttributes (_ :+ Attribute[A](value))
    def drop[A]()(implicit z: Key[A]): Metadata = if (has[A]) transformAttributes(_ filterNot (_ hasKey z)) else this

    def only[A : Key] = new Only[A]
    class Only[A : Key] {
      def map[B : LenientKey](f: A => B): Metadata = fold[A](ifValue = a => drop[A] set f(a), orElse = self)
      def mapOnly(f: A =?> A): Metadata            = map(x => if (f isDefinedAt x) f(x) else x)
      def flatMap(f: A => Option[A]): Metadata     = fold[A](ifValue = a => f(a).fold(self)(self set _), orElse = self)
      def filter(p: A => Boolean): Metadata        = fold[A](ifValue = a => if (!p(a)) drop[A] else self, orElse = self)
    }

    def fold[A]: Fold[A] = new Fold[A]
    class Fold[A] {
      def apply[B](ifValue: A => B, orElse: => B)(implicit z: Key[A]): B =
        if (has[A]) ifValue(untypedAs[A]) else orElse
    }

    def foreach(f: Any =?> Unit): Unit = attributes.map(_.value) foreach (x => if (f isDefinedAt x) f(x) else unit)

    def mapOnly(pf: Attribute =?> Attribute): Metadata = map(x => if (pf isDefinedAt x) pf(x) else x)
    def map(f: Attribute => Attribute): Metadata       = transformAttributes(_ map f)
    def flatMap(f: Attribute => Metadata): Metadata    = transformAttributes(_ flatMap (x => f(x).attributes))
    def filter(p: Attribute => Boolean): Metadata      = transformAttributes(_ filter p)

    def to_s = if (isEmpty) "{ }" else attributes mkString ("{\n  ", "\n  ", "\n}")
  }
  object Metadata {
    val empty: Metadata = new Metadata(Vector())
    def apply(attributes: Attribute*): Metadata = new Metadata(attributes.toVector)
  }

  /** Attribute is a dependently typed key/value pair.
   */
  trait Attribute extends Any with ShowDirect {
    type Type
    def key: Key[Type]
    def value: Type

    def hasSameKey(that: Attribute)      = key == that.key
    def hasKey[A](implicit that: Key[A]) = key == that

    def pair = key -> value
    def to_s = s"$key: $value"
  }

  object Attribute {
    type Of[A] = Attribute { type Type = A }

    import scala.language.implicitConversions
    implicit def apply[A](value: A)(implicit key: LenientKey[A]): Of[A] = AttributeOf[A](key, value)

    def unapply(x: Attribute): Some[x.Type] = Some(x.value)

    /** The simple implementation of Attribute, which ties the knot between the
     *  user-facing type parameter and the type member which couples key and value.
     */
    private case class AttributeOf[A](key: Key[A], value: A) extends Attribute with ShowSelf { type Type = A }
  }
}
