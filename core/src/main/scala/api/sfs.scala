package sfs
package api

trait Filesystem {

  type Id

  /** There are a number of ways we could arrange the "primitives"
   *  here. The most important thing is to retain the decoupling at
   *  abstraction boundaries so that one does not wind up computing
   *  things too eagerly, nor caching data for too long.
   */
  def resolve(path: Path): Id

  // for directories
  def resolve(path: Path, name: Name): Id

  def lookup(id: Id): Metadata

  // could be placed in another file, for ease of experimentation I put it here
  def update(id: Id, metadata: Metadata): Unit

  def relocate(oldId: Id, newId: Id): Unit
}
