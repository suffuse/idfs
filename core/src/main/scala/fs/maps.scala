package sfs
package fs

import api._, attributes._

sealed trait Map[-A, +B] extends (A => B)
case class NotOptimized[A, B](f: A => B) extends Map[A, B] {
  def apply(a: A) = f(a)
}
case class DataMap(f: Data => Data) extends Map[Metadata, Metadata] {
  def apply(metadata: Metadata) =
    metadata[Node] match {
      case File(data) =>
        val mapped = f(data.get)
        metadata set File(mapped) set Size(mapped.size)
      case _ => metadata
    }
}
case class DirMap(dirFunction: DirFunction) extends Map[Metadata, Metadata] {
  def apply(metadata: Metadata) =
    metadata.only[Node] mapOnly {
      case dir: Dir => dirFunction(dir)
    }
}
case class ExtensionMap(fromExt: String, toExt: String) extends Map[Name, Name] {
  def apply(name: Name) =
    if (name.extension == fromExt) name replaceExtension toExt
    else name
}

sealed trait DirFunction extends (Dir => Dir)
case class DirFilter(path: Path, predicate: Predicate[Path]) extends DirFunction {
  def apply(dir: Dir) = dir filter (name => predicate(path / name))
}
case class DirNamesMap(f: Map[Name, Name]) extends DirFunction {
  def apply(dir: Dir) = dir map f
}
