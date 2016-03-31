package sfs
package api

import jio._

trait Filesystem {

  def resolve(path: Path): Metadata
}

trait Store {
  def update(path: Path, metadata: Metadata): Unit

  def move(oldPath: Path, newPath: Path): Unit
}

object Filesystem {
  implicit class FilesystemOps(val fs: Filesystem) extends AnyVal {

    def map(f: (Path => Metadata) => (Path => Metadata)) =
      new Filesystem {
        def resolve(path: Path) =
          f(fs.resolve) apply path
      }

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

    def resolve(path: String): Metadata =
      fs resolve toPath(path)
  }
}

object Store {
  implicit class StoreOps(val store: Store) extends AnyVal {
    def move(oldPath: String, newPath: String) =
      store move (toPath(oldPath), toPath(newPath))

    def update(path: String, metadata: Metadata) =
      store update (toPath(path), metadata)
  }
}
