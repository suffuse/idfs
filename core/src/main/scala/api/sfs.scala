package sfs
package api

trait Filesystem {

  /** A Path is a serialization of the steps one must take from
   *  the root to a particular node in the tree.
   */
  type Path

  implicit val pathKey = new Key[Path]("path")

  /** Some means of performing I/O on a virtualized file.
   */
  type Data

  /** There are a number of ways we could arrange the "primitives"
   *  here. The most important thing is to retain the decoupling at
   *  abstraction boundaries so that one does not wind up computing
   *  things too eagerly, nor caching data for too long.
   */
  def resolve(path: Path): Metadata

  // could be placed in another file, for ease of experimentation I put it here
  def update(path: Path, metadata: Metadata): Unit

  // not the way to go, but adding a 'wrapping' data type for
  // laziness, you probably have something lying around for this
  sealed class Lazy[A](fetch: => A) {
    lazy val get = fetch
  }
  object Lazy {
    def apply[A](fetch: => A) = new Lazy(fetch)
  }

  sealed trait Node extends AnyRef
  object Node {
    implicit val _node = new Key[Node]("node")
    implicit def _empty: Empty[Node] = Empty(NoNode)
  }
  final case object NoNode                            extends Node
  final case class  Link(target: Path)                extends Node
  final case class  File(data: Lazy[Data])            extends Node
  // kids are wrapped in lazy because the one retrieving the metadata of the dir
  // might not have access to the kids
  final case class  Dir (kids: Set[Name]) extends Node {
    def filter(p: Name => Boolean): Dir =
      Dir(kids filter p)
  }

  object File extends (Lazy[Data] => File) {
    def apply(data: => Data): File = File(Lazy(data))

    implicit def empty(implicit z:Empty[Data]): Empty[File] = Empty(File(z.emptyValue))
  }
  object Dir extends (Set[Name] => Dir) {
    implicit def empty: Empty[Dir] = Empty(Dir(Set.empty[Name]))
  }
}
