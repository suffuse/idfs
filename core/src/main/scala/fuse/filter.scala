package sfs
package fuse

import jio._


class FilteredDirFiller(filler: DirectoryFiller, condition: String => Boolean) extends DirectoryFiller {
  def add(files: jIterable[String]): Boolean = filler add (files.asScala filter condition).asJava
  def add(files: String*): Boolean           = filler add (files filter condition).asJava
}
class MappedDirFiller(filler: DirectoryFiller, f: String => String) extends DirectoryFiller {
  def add(files: jIterable[String]): Boolean = filler add (files.asScala map f).asJava
  def add(files: String*): Boolean           = filler add (files map f).asJava
}
