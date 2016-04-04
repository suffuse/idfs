package sfs
package api

import jio._, attributes._

trait Filesystem {
  def apply[A](action: Action[A]): A
}

object Filesystem {

  implicit class FilesystemOps(fs: Filesystem) { ops =>

    class TransformedFilesystem(transformer: Action ~> Action) extends Filesystem {
      def apply[A](action: Action[A]): A = fs apply transformer(action)
      def andThen(next: Action ~> Action) = new TransformedFilesystem(transformer andThen next)
    }

    def transform(transformer: Action ~> Action) = new TransformedFilesystem(transformer)

    def map(f: Metadata => Metadata) =
      this transform new Transformer {
        def transform[A] = { case action: Resolve => Transformation.Map(action, f) }
      }

    def mapNode(f: Node =?> Node) =
      map(_.only[Node] mapOnly f)

    def filter(p: Path => Boolean) =
      this transform new Transformer {
        def transform[A] = { case action: Resolve => Transformation.FilterPath(action, p) }
      }

    def filterNot(p: Path => Boolean) = filter(x => !p(x))
  }
}

sealed trait Action[A] {
  def flatMap[B](f: A => Action[B]): Action[B] = FlatMap(this, f)
  def map[B](f: A => B): Action[B]             = FlatMap(this, f andThen InstantResult[B])
}

sealed trait Machinery[A] extends Action[A]
case class FlatMap[A, B](action: Action[A], f: A => Action[B]) extends Machinery[B]
case class InstantResult[A](a: A)                              extends Machinery[A]

sealed trait Transformation[A] extends Action[A] {
  def defaultResult: Action[A]
  def action: Action[A]
}
sealed trait PathActionTransformation[A] extends PathAction[A] with Transformation[A] {
  def action: PathAction[A]
  def path = action.path
  def mapPath(f: Path => Path) = action mapPath f
}
object Transformation {
  case class FilterPath(action: PathAction[Metadata], predicate: Path => Boolean) extends PathActionTransformation[Metadata] {
    def defaultResult =
      if (predicate(path))
        action.map(_.only[Node].mapOnly {
          case x: Dir => x filter (name => predicate(path / name))
        })
      else InstantResult(empty[Metadata])
  }
  case class Map(action: PathAction[Metadata], f: Metadata => Metadata) extends PathActionTransformation[Metadata] {
    def defaultResult = action map f
  }
}

sealed trait PathAction[A] extends Action[A] {
  def path: Path
  def mapPath(f: Path => Path): PathAction[A]
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

trait ConcreteActionsOnly { _: Filesystem =>

  protected def handleConcreteAction[A](action: ConcreteAction[A]): A

  def apply[A](action: Action[A]): A =
    action match {
      case a: ConcreteAction[A] => handleConcreteAction(a)
      case InstantResult(a)     => a
      case FlatMap(a, f)        => apply(apply(a) |> f)
      case a: Transformation[A] => apply(a.defaultResult)
    }
}

trait Transformer extends (Action ~> Action) {
  def apply[A](action: Action[A]): Action[A] = if (transform isDefinedAt action) transform(action) else action
  def transform[A]: Action[A] =?> Action[A]
}

object RemoveWrites extends Transformer {
  def transform[A] = {
    case action: Resolve => action map (_.only[UnixPerms] map (_.noWrites))
  }
}
