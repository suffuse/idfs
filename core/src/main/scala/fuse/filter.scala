package sfs
package fuse

import jio._

/** Forwarding filesystem which only passes through paths which match the filter.
 */
class FilteredFs(protected val underlying: FuseFilesystem, cond: String => Boolean) extends ForwarderFs {
  override def readdir(path: String, df: DirectoryFiller): Int =
    underlying.readdir(path, new FilteredDirFiller(df, cond))
}

class FilteredDirFiller(filler: DirectoryFiller, condition: String => Boolean) extends DirectoryFiller {
  def add(files: jIterable[String]): Boolean = filler add (files.asScala filter condition).asJava
  def add(files: String*): Boolean           = filler add (files filter condition).asJava
}

object filterfs {
  def main(args: Array[String]): Unit = args.toList match {
    case from :: to :: regex :: Nil => idfs(path(from)) filterNot (_ matches regex) mountForeground path(to)
    case _                          => println("Usage: filter <from> <to> <regex>")
  }
}
