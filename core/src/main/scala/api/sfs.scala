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

  type Id

  /** There are a number of ways we could arrange the "primitives"
   *  here. The most important thing is to retain the decoupling at
   *  abstraction boundaries so that one does not wind up computing
   *  things too eagerly, nor caching data for too long.
   */
  def resolve(path: Path): Id

  def lookup(id: Id): Metadata

  // could be placed in another file, for ease of experimentation I put it here
  def update(id: Id, metadata: Metadata): Unit

  def relocate(oldId: Id, newId: Id): Unit

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
  final case object NoNode                   extends Node
  // target was a `Path`, that does not make sense. There seem to be two choices:
  // 1. An arbitrary value, allowing links to point at things outside of the file system
  // 2. An `Id`, making sure the link points to a node within the file system
  // Fuse (and Java) both support absolute and relative links, making #2 a bit tricky to
  // implement. For that reason I chose for #1
  final case class  Link(target: LinkTarget) extends Node
  final case class  File(data: Lazy[Data])   extends Node
  // We could choose for a dir to have kids typed as `Id`, making sure they are part of
  // the file system
  final case class  Dir (kids: Map[Name, Id])    extends Node {
    def filter(p: Name => Boolean): Dir =
      Dir(kids filter { case (name, _) => p(name) })
  }

  object File extends (Lazy[Data] => File) {
    def apply(data: => Data): File = File(Lazy(data))

    implicit def empty(implicit z:Empty[Data]): Empty[File] = Empty(File(z.emptyValue))
  }
  object Dir extends (Map[Name, Id] => Dir) {
    implicit def empty: Empty[Dir] = Empty(Dir(Map.empty[Name, Id]))
  }
}
