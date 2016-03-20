package sfs
package fuse

import jio._

object idfs extends FsRunner {
  def runMain = { case Array(from, to) =>
    start(new Rooted(from).logging, to)
  }
}
object filterfs extends FsRunner {
  override def usage = "<from> <to> <regex>"
  def runMain = { case Array(from, to, regex) =>
    start(new Rooted(from) filterNot (_ matches regex), to)
  }
}
object reversefs extends FsRunner {
  trait Reverser extends RootedFs {
    override protected def pathBytes(path: Path): Array[Byte] =
      super.pathBytes(path).reverse
  }
  def runMain = { case Array(from, to) =>
    start(new Rooted(from) with Reverser, to)
  }
}

/** Reverses all the data on the filesystem.
 */
/** Generic SFS runner.
 */
abstract class FsRunner {
  def runMain: Array[String] =?> Unit

  def name: String                                = this.getClass.shortName
  def usage: String                               = "<from> <to>"
  def start(fs: FuseFs, mountPoint: String): Unit = fs mountForeground path(mountPoint)

  class Rooted(val root: Path) extends RootedFs {
    def this(root: String) = this(path(root))
    def getName = name
  }

  def main(args: Array[String]): Unit = {
    if (runMain isDefinedAt args) runMain(args)
    else Console.err.println(s"Usage: $name $usage")
  }
}
