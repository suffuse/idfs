package sfs
package api

trait Filesystem {

  def resolve(path: Path): Metadata

  def update(path: Path, metadata: Metadata): Unit

  def move(oldPath: Path, newPath: Path): Unit
}

object Filesystem {
  import scala.language.implicitConversions
  implicit def FilesystemOps(u: Filesystem) = new fs.FilesystemOps { val fs: u.type = u }
}
