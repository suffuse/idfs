package sfs
package api

trait Filesystem {

  /** A Path is a serialization of the steps one must take from
   *  the root to a particular node in the tree.
   */
  type Path

  /** Some means of performing I/O on a virtualized file.
   */
  type IO

  /** There are a number of ways we could arrange the "primitives"
   *  here. The most important thing is to retain the decoupling at
   *  abstraction boundaries so that one does not wind up computing
   *  things too eagerly, nor caching data for too long.
   */
  def resolve(path: Path): Metadata

  // not the way to go, but adding a 'wrapping' data type
  // should be `Eval` like, you probably have something
  // lying around for this
  sealed class Data[A](fetch: => A) {
    lazy val io = fetch
  }
  object Data {
    def apply[A](fetch: => A) = new Data(fetch)
  }

  sealed trait Node extends AnyRef
  object Node {
    implicit val _data = new Key[Node]("node")
    implicit def _empty: Empty[Node] = Empty(NoNode)
  }
  final case object NoNode                            extends Node
  final case class  Link(target: Path)                extends Node
  final case class  File(data: Data[IO])              extends Node
  // kids are wrapped in data because the one retrieving the metadata of the dir
  // might not have access to the kids
  final case class  Dir (kids: Data[Map[Name, Path]]) extends Node {
    def filter(p: Path => Boolean): Dir =
      Dir(Data(kids.io filter { case (_, path) => p(path) }))
  }
}
