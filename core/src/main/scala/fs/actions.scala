package sfs
package fs

import api._, attributes._

sealed trait Action[A]
object Action {
  implicit class ActionOps[A](val action: Action[A]) extends AnyVal {
    def map[B](f: A => B): Action[B]              = FlatMapAction(action, f andThen InstantResult[B])
    def flatMap[B](f: A => Action[B]): Action[B]  = FlatMapAction(action, f)
  }

  implicit class MetadataActionOps(val action: Action[Metadata]) extends AnyVal {

    def mapExtensionsInDir(from: String, to: String) =
      mapDir(_ map (name => if (name.extension == from) name replaceExtension to else name))

    def mapDataOfFiles(using: Data => Data) =
      action map { metadata =>
        metadata[Node] match {
          case File(data) =>
            val mapped = using(data.get)
            metadata set File(mapped) set Size(mapped.size)
          case _ => metadata
        }
      }

    def filterDir(path: Path, predicate: Path => Boolean) =
      mapDir(_ filter (name => predicate(path / name)))

    def mapDir(using: Dir => Dir) =
      action map (
        _.only[Node] mapOnly {
          case dir: Dir => using(dir)
        }
      )

    def noWrites =
      action map (_.only[UnixPerms] map (_.noWrites))
  }
}

case class InstantResult[A](result: A)                               extends Action[A]
case class FlatMapAction[A, B](action: Action[A], f: A => Action[B]) extends Action[B]

sealed abstract class ConcreteAction[A : Empty] extends Action[A] {
  def emptyResult: A = empty[A]
  def path: Path
  def mapPath(f: Path => Path): ConcreteAction[A]
}

sealed abstract class EffectAction extends ConcreteAction[Unit]

case class Resolve   (path: Path)                         extends ConcreteAction[Metadata] { def mapPath(f: Path => Path) = copy(path = f(path)) }
case class CreateFile(path: Path, permissions: UnixPerms) extends EffectAction             { def mapPath(f: Path => Path) = copy(path = f(path)) }
case class CreateDir (path: Path, permissions: UnixPerms) extends EffectAction             { def mapPath(f: Path => Path) = copy(path = f(path)) }
case class CreateLink(path: Path, target: String)         extends EffectAction             { def mapPath(f: Path => Path) = copy(path = f(path)) }
case class Update    (path: Path, metadata: Metadata)     extends EffectAction             { def mapPath(f: Path => Path) = copy(path = f(path)) }
case class Move      (path: Path, to: Path)               extends EffectAction             { def mapPath(f: Path => Path) = copy(path = f(path)) }
case class Write     (path: Path, data: Data)             extends EffectAction             { def mapPath(f: Path => Path) = copy(path = f(path)) }
case class Remove    (path: Path)                         extends EffectAction             { def mapPath(f: Path => Path) = copy(path = f(path)) }
