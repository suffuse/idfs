package sfs
package fs

import api._

trait FilesystemOps {

  val fs: Filesystem

  def map(toNewMetadata: Metadata => Metadata, fromNewMetadata: Metadata => Metadata) =
    new Filesystem {

      def resolve(path: Path) =
        (fs resolve path) |> toNewMetadata

      def update(path: Path, metadata: Metadata): Unit =
        fs update (path, metadata |> fromNewMetadata)

      def move(oldPath: Path, newPath: Path) =
        fs move (oldPath, newPath)
    }

  def mapNode(f: Node =?> Node) = map(_.only[Node] mapOnly f, identity)

  def filter(p: Name => Boolean) = mapNode {
    case dir: Dir => dir filter p
  }
  def filterNot(p: Name => Boolean) = filter(x => !p(x))

  def resolve(path: String): Metadata =
    fs resolve toPath(path)

  def update(path: String, metadata: Metadata): Unit =
    fs update (toPath(path), metadata)

  def move(oldPath: String, newPath: String): Unit =
    fs move (toPath(oldPath), toPath(newPath))
}
