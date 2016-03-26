package sfs
package api

trait Filesystem {

  /** A Path is a serialization of the steps one must take from
   *  the root to a particular node in the tree.
   */
  type Path

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
  final case class  Dir (kids: Lazy[Map[Name, Path]]) extends Node {
    def filter(p: Path => Boolean): Dir =
      Dir(Lazy(kids.get filter { case (_, path) => p(path) }))
  }

  object File extends (Lazy[Data] => File) {
    def apply(data: => Data): File = File(Lazy(data))
  }
  object Dir extends (Lazy[Map[Name, Path]] => Dir) {
    def apply(kids: => Map[Name, Path]): Dir = Dir(Lazy(kids))

    implicit def empty: Empty[Dir] = Empty(Dir(Map.empty[Name, Path]))
  }
}
