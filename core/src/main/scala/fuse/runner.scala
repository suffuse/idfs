package sfs
package fuse

import api._, fs._

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

  def runMain = { case Array(from, to) =>
    val rooted = new Rooted(from)
    import rooted.fs
    start(
      rooted.mapNode {
        case File(data) => File(data.get.reverse)
      },
      to
    )
  }
}

/** Generic SFS runner.
 */
abstract class FsRunner {
  def runMain: Array[String] =?> Unit

  def name: String                                = this.getClass.shortName
  def usage: String                               = "<from> <to>"
  def start(fs: FuseFs, mountPoint: String): Unit = fs mountForeground toPath(mountPoint)

  class Rooted(val fs: Filesystem) extends RootedFs {
    def this(root: Path) = this(new jio.JavaFilesystem(root))
    def this(root: String) = this(toPath(root))
    def getName = name

    def filterNot(p: String => Boolean) = new Rooted(fs filterNot p)
    def mapNode(f: Node =?> Node)       = new Rooted(fs mapNode f)
  }

  def main(args: Array[String]): Unit = {
    if (runMain isDefinedAt args) runMain(args)
    else Console.err.println(s"Usage: $name $usage")
  }
}
