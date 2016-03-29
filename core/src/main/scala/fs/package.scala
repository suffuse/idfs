package sfs

import api._

package object fs {

  implicit def Wrapped(u: Filesystem) = new WrappedFileSystem { val fs: u.type = u }

  // This file exists to experiment with transforming parts of the file system, in this case the Path
  trait WrappedFileSystem {

    val fs: Filesystem

    def map(toNewMetadata: Metadata => Metadata, fromNewMetadata: Metadata => Metadata) =
      new Filesystem {
        type Id = fs.Id

        def resolve(path: Path) =
          fs resolve path

        def resolve(path: Path, name: Name): Id =
          fs resolve (path, name)

        def lookup(id: Id) =
          (fs lookup id) |> toNewMetadata

        def update(id: Id, metadata: Metadata): Unit =
          fs update (id, metadata |> fromNewMetadata)

        def relocate(oldId: Id, newId: Id) =
          fs relocate (oldId, newId)
      }

    def mapNode(f: Node =?> Node) = map(_.only[Node] mapOnly f, identity)

    def filter(p: Name => Boolean) = mapNode {
      case dir: Dir => dir filter p
    }
    def filterNot(p: Name => Boolean) = filter(x => !p(x))
  }
}
