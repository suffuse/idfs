package sfs
package api

trait Filesystem {
  /** A Path is a serialization of the steps one must take from
   *  the root to a particular node in the tree. A Name is a single
   *  directed edge of the graph.
   */
  type Path
  type Name

  /** A key of some kind, only meaningful within this filesystem,
   *  which uniquely pinpoints the entity to which a path resolves.
   *  It is decoupled from the name.
   */
  type Key

  /** Some means of performing I/O on a virtualized file.
   */
  type IO

  /** There are a number of ways we could arrange the "primitives"
   *  here. The most important thing is to retain the decoupling at
   *  abstraction boundaries so that one does not wind up computing
   *  things too eagerly, nor caching data for too long.
   */
  def resolve(path: Path): Key
  def metadata(key: Key): Metadata
  def lookup(key: Key): Data

  sealed trait Data                              extends AnyRef
  final case class File(io: IO)                  extends Data
  final case class Dir(children: Map[Name, Key]) extends Data
  final case class Link(target: Path)            extends Data
  final case class Other(io: IO)                 extends Data
  final case object None                         extends Data
}

trait UnixLikeFilesystem extends Filesystem {
  /** These could have stronger types - this is the minimal example.
   */
  type Path = String
  type Name = String       // assert(name forall (ch != '/'))
  type Key  = Long         // 64-bit inode
  type IO   = Array[Byte]  // for maximum simplicity, but even unix isn't this simple
}
