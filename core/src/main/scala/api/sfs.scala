package sfs
package api

import jio._, attributes._

trait Filesystem {
  def apply[A](action: Action[A]): A
}

object Filesystem {

  implicit class FilesystemOps(fs: Filesystem) { ops =>

    def transform(transformer: Transformer) =
      new Filesystem {
        def apply[A](action: Action[A]): A =
          fs apply transformer(action)
      }

    object reads {

      def removeWrites =
        new Transformer {
          def transform[A] = {
            case action: Resolve => action map (_.only[UnixPerms] map (_.noWrites))
          }
        }

      def transform(transformer: Transformer) =
        ops transform (transformer andThen removeWrites)

      def map(f: Metadata => Metadata) =
        this transform new Transformer {
          def transform[A] = { case action: Resolve => action map f }
        }

      def mapNode(f: Node =?> Node) =
        map(_.only[Node] mapOnly f)

      def filter(p: Name => Boolean) =
        this transform new Transformer {
          def transform[A] = {
            case action @ Resolve(path) if p(path.lastSegment) =>
              action.map(_.only[Node].map {
                case x: Dir => x
                case x => x // I would have used `mapOnly` if the compiler allowed me
              })

            case action: Resolve =>
              InstantResult(empty[Metadata])
          }
        }

      def filterNot(p: Name => Boolean) = filter(x => !p(x))

    }
  }
}

sealed trait Action[A] {
  def flatMap[B](f: A => Action[B]): Action[B] = FlatMap(this, f)
  def map[B](f: A => B): Action[B]             = FlatMap(this, f andThen InstantResult[B])
}

sealed trait Machinery[A] extends Action[A]
case class FlatMap[A, B](action: Action[A], f: A => Action[B]) extends Machinery[B]
case class InstantResult[A](a: A)                              extends Machinery[A]

sealed abstract class FsAction[A]()(implicit z: Empty[A]) extends Action[A] { self =>
  def path: Path
  def mapPath(f: Path => Path): FsAction[A]
  def empty: A = z.emptyValue
}
case class Resolve   (path: Path)                         extends FsAction[Metadata] { def mapPath(f: Path => Path) = copy(path = f(path)) }
case class CreateFile(path: Path, permissions: UnixPerms) extends FsAction[Unit]     { def mapPath(f: Path => Path) = copy(path = f(path)) }
case class CreateDir (path: Path, permissions: UnixPerms) extends FsAction[Unit]     { def mapPath(f: Path => Path) = copy(path = f(path)) }
case class CreateLink(path: Path, target: String)         extends FsAction[Unit]     { def mapPath(f: Path => Path) = copy(path = f(path)) }
case class Update    (path: Path, metadata: Metadata)     extends FsAction[Unit]     { def mapPath(f: Path => Path) = copy(path = f(path)) }
case class Move      (path: Path, to: Path)               extends FsAction[Unit]     { def mapPath(f: Path => Path) = copy(path = f(path)) }
case class Write     (path: Path, data: Data)             extends FsAction[Unit]     { def mapPath(f: Path => Path) = copy(path = f(path)) }
case class Remove    (path: Path)                         extends FsAction[Unit]     { def mapPath(f: Path => Path) = copy(path = f(path)) }

trait FsActionsOnly { _: Filesystem =>

  protected def handleFsAction[A](action: FsAction[A]): A

  def apply[A](action: Action[A]): A =
    action match {
      case a: FsAction[A]   => handleFsAction(a)
      case InstantResult(a) => a
      case FlatMap(a, f)    => apply(apply(a) |> f)
    }
}

trait Transformer { self =>
  def apply[A](action: Action[A]): Action[A] = if (transform isDefinedAt action) transform(action) else action
  def transform[A]: Action[A] =?> Action[A]
  def andThen(transformer: Transformer): Transformer =
    new Transformer {
      def transform[A] = {
        case action => transformer(self(action))
      }
    }
}
