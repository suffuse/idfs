package sfs

import api._

package object fs {

  implicit def Wrapped(u: Filesystem) = new WrappedFileSystem { val fs: u.type = u }

  // This file exists to experiment with transforming parts of the file system, in this case the Path
  trait WrappedFileSystem {

    val fs: Filesystem

    def map(toNewMetadata: Metadata => Metadata, fromNewMetadata: Metadata => Metadata) =
      new Filesystem with CompatibleNodes {
        type Path = fs.Path

        def resolve(path: Path) =
          fs resolve path

        def lookup(id: Id) =
          (fs lookup id) |> fromFsNodes |> toNewMetadata

        def update(id: Id, metadata: Metadata): Unit =
          fs update (id, metadata |> fromNewMetadata |> toFsNodes)

        def relocate(oldId: Id, newId: Id) =
          fs relocate (oldId, newId)
      }

    def contraMap[T](toNewPath: fs.Path => T, fromNewPath: T => fs.Path) =
      new Filesystem with CompatibleNodes {
        type Path = T

        def resolve(path: Path) =
          fs resolve fromNewPath(path)

        def lookup(id: Id) =
          (fs lookup id) |> fromFsNodes

        def update(id: Id, metadata: Metadata): Unit =
          fs update (id, toFsNodes(metadata))

        def relocate(oldId: Id, newId: Id) =
          fs relocate (oldId, newId)
      }

    def withMappedPath[T](toNewPath: fs.Path => T, fromNewPath: T => fs.Path) =
      contraMap(toNewPath, fromNewPath)

    def mapNode(f: fs.Node =?> fs.Node) = map(_.only[fs.Node] mapOnly f, identity)

    def filter(p: Name => Boolean) = mapNode {
      case dir: fs.Dir => dir filter p
    }
    def filterNot(p: Name => Boolean) = filter(x => !p(x))

    trait CompatibleNodes { _: Filesystem =>
      type Data = fs.Data
      type Id   = fs.Id

      def fromFsNodes: Metadata => Metadata =
        _.only[fs.Node].map {
          case fs.File(data)   => File(data.get)
          case fs.Dir (kids)   => Dir (kids)
          case fs.Link(target) => Link(target)
          case fs.NoNode       => NoNode
        }

      def toFsNodes: Metadata => Metadata =
        _.only[Node].map {
          case File(data)   => fs.File(data.get)
          case Dir (kids)   => fs.Dir(kids)
          case Link(target) => fs.Link(target)
          case NoNode       => fs.NoNode
        }
    }

  }
}
