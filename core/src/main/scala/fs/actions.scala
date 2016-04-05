package sfs
package fs

import api._, attributes._
import scala.util.matching.Regex

// just a marker we can use to optimize stuff
sealed trait `Are you sure?`
object `I don't feel like optimizing` {
  implicit val really: `Are you sure?` = null
}

sealed trait Action[A] {
  def flatMap[B](f: A => Action[B])(implicit z: `Are you sure?`): Action[B] = FlatMapAction(this, f)
  def map[B](f: A => B)(implicit z: `Are you sure?`): Action[B]             = MapAction(this, NotOptimized(f))
}

case class InstantResult[A](a: A)                                    extends Action[A]
case class MapAction[A, B](action: Action[A], f: Map[A, B])          extends Action[B]
case class FlatMapAction[A, B](action: Action[A], f: A => Action[B]) extends Action[B]

// Allows an implementing file system to decompose the requested transformation and supply
// a more efficient implementation. A filter could, for example, be transformed into a
// where clause if the file system happens to be backed by a database
//
// In other cases, or when experimenting, the `defaultResult` can be used
sealed trait Transformation[A] extends Action[A] {
  def defaultResult: Action[A]
  def action: Action[A]
}

sealed trait PathAction[A] extends Action[A] {
  def path: Path
  def mapPath(f: Path => Path): PathAction[A]
}

sealed trait PathActionTransformation[A] extends PathAction[A] with Transformation[A] {
  def action: PathAction[A]
  def path = action.path
  def mapPath(f: Path => Path) = action mapPath f
}

sealed abstract class ConcreteAction[A : Empty] extends PathAction[A] {
  def emptyResult: A = empty[A]
}

case class Resolve   (path: Path)                         extends ConcreteAction[Metadata] { def mapPath(f: Path => Path) = copy(path = f(path)) }
case class CreateFile(path: Path, permissions: UnixPerms) extends ConcreteAction[Unit]     { def mapPath(f: Path => Path) = copy(path = f(path)) }
case class CreateDir (path: Path, permissions: UnixPerms) extends ConcreteAction[Unit]     { def mapPath(f: Path => Path) = copy(path = f(path)) }
case class CreateLink(path: Path, target: String)         extends ConcreteAction[Unit]     { def mapPath(f: Path => Path) = copy(path = f(path)) }
case class Update    (path: Path, metadata: Metadata)     extends ConcreteAction[Unit]     { def mapPath(f: Path => Path) = copy(path = f(path)) }
case class Move      (path: Path, to: Path)               extends ConcreteAction[Unit]     { def mapPath(f: Path => Path) = copy(path = f(path)) }
case class Write     (path: Path, data: Data)             extends ConcreteAction[Unit]     { def mapPath(f: Path => Path) = copy(path = f(path)) }
case class Remove    (path: Path)                         extends ConcreteAction[Unit]     { def mapPath(f: Path => Path) = copy(path = f(path)) }

case class FilterPath(action: PathAction[Metadata], predicate: Predicate[Path]) extends PathActionTransformation[Metadata] {
  def defaultResult =
    if (predicate(path)) MapAction(action, DirMap(DirFilter(path, predicate)))
    else InstantResult(empty[Metadata])
}
