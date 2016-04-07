package sfs
package fs

import api._, attributes._

sealed trait Map[A, B] /* do not extend (A => B) */ {
  def asFunction: A => B
}
case class NotOptimized[A, B](f: A => B) extends Map[A, B] {
  def asFunction = f
}
case class DataMap(f: Data => Data) extends Map[Metadata, Metadata] {
  def asFunction = metadata =>
    metadata[Node] match {
      case File(data) =>
        val mapped = f(data.get)
        metadata set File(mapped) set Size(mapped.size)
      case _ => metadata
    }
}
case class DirMap(dirFunction: DirFunction) extends Map[Metadata, Metadata] {
  def asFunction = metadata =>
    metadata.only[Node] mapOnly {
      case dir: Dir => dirFunction(dir)
    }
}
case object NoWrites extends Map[Metadata, Metadata] {
  def asFunction = _.only[UnixPerms] map (_.noWrites)
}
case class ExtensionMap(from: String, to: String) extends Map[Name, Name] {
  def asFunction = name =>
    if (name.extension == from) name replaceExtension to
    else name
}

sealed trait DirFunction extends (Dir => Dir)
case class DirFilter(path: Path, predicate: Predicate[Path]) extends DirFunction {
  def apply(dir: Dir) = dir filter (name => predicate asFunction (path / name))
}
case class DirNamesMap(m: Map[Name, Name]) extends DirFunction {
  def apply(dir: Dir) = dir map m.asFunction
}
