package sfs
package fs

import api._

trait FilesystemOps {

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

  def resolve(path: String): fs.Id =
    fs resolve toPath(path)

  def lookup(path: Path): Metadata =
    fs lookup (fs resolve path)

  def lookup(path: String): Metadata =
    lookup(toPath(path))
}
