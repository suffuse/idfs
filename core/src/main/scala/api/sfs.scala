package sfs
package api

import jio._, attributes._

trait Filesystem {

  def resolve(path: Path): Metadata

  def update(path: Path, metadata: Metadata): Unit

  def move(oldPath: Path, newPath: Path): Unit
}

object Filesystem {
  implicit class FilesystemOps(fs: Filesystem) { ops =>

    def map(
      sourceToUser: (Path => Metadata) => (Path => Metadata),
      userToSource: ((Path, Metadata)) => (Path, Metadata)
    ) = {

      new Filesystem {
        def resolve(path: Path) =
          sourceToUser(fs.resolve) apply path

        def update(path: Path, metadata: Metadata): Unit = {
          val (p, m) = userToSource(path, metadata)
          fs update (p, m)
        }

        def move(oldPath: Path, newPath: Path): Unit =
          fs move (oldPath, newPath)
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

    def resolve(path: String): Metadata =
      fs resolve toPath(path)

    def move(oldPath: String, newPath: String) =
      fs move (toPath(oldPath), toPath(newPath))

    def update(path: String, metadata: Metadata) =
      fs update (toPath(path), metadata)
  }
}
