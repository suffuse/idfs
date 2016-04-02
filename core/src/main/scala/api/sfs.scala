package sfs
package api

import jio._, attributes._

sealed trait Update
case class CreateFile(permissions: UnixPerms) extends Update
case class CreateDir (permissions: UnixPerms) extends Update
case class CreateLink(target: String)         extends Update
case class UpdateAttribute(attr: Attribute)   extends Update
case class Move(to: Path)                     extends Update
case class Write(data: Data)                  extends Update
case class Multiple(updates: Seq[Update])     extends Update
case object Remove                            extends Update

trait Filesystem {

  def resolve(path: Path): Metadata

  def update(path: Path, update: Update): Unit
}

object Filesystem {
  implicit class FilesystemOps(fs: Filesystem) { ops =>

    def map(
      sourceToUser: (Path => Metadata) => (Path => Metadata),
      userToSource: ((Path, Update)) => (Path, Update)
    ) = {

      new Filesystem {
        def resolve(path: Path) =
          sourceToUser(fs.resolve) apply path

        def update(path: Path, update: Update): Unit = {
          val (p, u) = userToSource(path, update)
          fs update (p, u)
        }
      }
    }

    object reads {

      def map(f: (Path => Metadata) => (Path => Metadata)) =
        ops.map(f andThen (_ andThen (_.only[UnixPerms] map (_.noWrites))), identity)

      def mapNode(f: Node =?> Node) =
        map(_ andThen (_.only[Node] mapOnly f))

      def filter(p: Name => Boolean) =
        map(resolve => {
          case path if p(path.lastSegment) =>
            resolve(path).only[Node] mapOnly {
              case dir: Dir => dir filter p
            }
          case _ => Metadata
        })

      def filterNot(p: Name => Boolean) = filter(x => !p(x))

    }
  }
}
