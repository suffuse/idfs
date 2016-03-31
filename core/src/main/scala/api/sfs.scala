package sfs
package api

trait Filesystem {

  def resolve(path: Path): Metadata
}

trait Store {
  def update(path: Path, metadata: Metadata): Unit

  def move(oldPath: Path, newPath: Path): Unit
}

object Filesystem {
  implicit class FilesystemOps(val fs: Filesystem) extends AnyVal {

    def map(f: Metadata => Metadata) =
      new Filesystem {

        def resolve(path: Path) =
          f(fs resolve path)
      }

    def mapNode(f: Node =?> Node) = map(_.only[Node] mapOnly f)

    def filter(p: Name => Boolean) = mapNode {
      case dir: Dir => dir filter p
    }
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
