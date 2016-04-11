package sfs
package fs

import api._, attributes._

sealed trait Map[-A, +B] /* do not extend (A => B) */ {
  def asFunction: A => B
  def andThen[C](next: Map[B, C]): Map[A, C] = Chained(this, next)
}
case class Chained[A, B, C](first: Map[A, B], second: Map[B, C]) extends Map[A, C] {
  def asFunction = first.asFunction andThen second.asFunction
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
      case dir: Dir => dirFunction.asFunction apply dir
    }
}
case object GetDir extends Map[Metadata, Dir] {
  def asFunction = _[Node] match {
    case dir: Dir => dir
    case _        => empty[Dir]
  }
}
case object SetDir extends Map[(Metadata, Dir), Metadata] {
  def asFunction = { case (m, d) => m set d }
}
case object NoWrites extends Map[Metadata, Metadata] {
  def asFunction = _.only[UnixPerms] map (_.noWrites)
}
case class ExtensionMap(from: String, to: String) extends Map[Name, Name] {
  def asFunction = name =>
    if (name.extension == from) name replaceExtension to
    else name
}
case class FsMap[A](fs: Filesystem) extends Map[Action[A], A] {
  def asFunction = fs.apply
}

sealed trait DirFunction extends Map[Dir, Dir]
case class DirFilter(path: Path, predicate: Predicate[Path]) extends DirFunction {
  def asFunction = _ filter (name => predicate asFunction (path / name))
}
case class DirNamesMap(m: Map[Name, Name]) extends DirFunction {
  def asFunction = _ map m.asFunction
}
case object DirConcat extends Map[(Dir, Dir), Dir] {
  def asFunction = { case (a, b) => a ++ b }
}
