package sfs
package fs

import api._, attributes._

sealed trait Action[A]
object Action {
  implicit class ActionOps[A](val action: Action[A]) extends AnyVal {
    def map[B](m: Map[A, B]): Action[B] = MapAction(action, m)
  }
}

object syntax {
  implicit class ForComprehensionSuport[A](val action: Action[A]) extends AnyVal {
    def flatMap[B](f: A => Action[B]): Action[B] = FlatMapAction(action, NotOptimized(f))
    def map[B](f: A => B): Action[B]             = MapAction(action, NotOptimized(f))
  }
}

case class InstantResult[A](result: A)                                  extends Action[A]
case class MapAction[A, B](action: Action[A], f: Map[A, B])             extends Action[B]
case class FlatMapAction[A, B](action: Action[A], f: Map[A, Action[B]]) extends Action[B]

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
    if (predicate.asFunction(path)) action map filterDir(path, predicate)
    else InstantResult(empty[Metadata])
}
