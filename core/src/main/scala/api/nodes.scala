package sfs
package api

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
final case class  Dir (kids: Set[Name])    extends Node {
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

// not the way to go, but adding a 'wrapping' data type for
// laziness, you probably have something lying around for this
final class Lazy[A](fetch: => A) {
  lazy val get = fetch
}
object Lazy {
  def apply[A](fetch: => A) = new Lazy(fetch)
}
