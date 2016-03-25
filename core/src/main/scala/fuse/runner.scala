package sfs
package fuse

import jio._, api._, fs._

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
        case fs.File(data) => fs.File(data.get.reverse)
      },
      to
    )
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

  // clicking different parts together
  def fuseJavaFs(root: Path) =
    new jio.JavaFilesystem(root) withMappedPath (_.to_s, path)

  class Rooted(val root: Path, val fs: FuseCompatibleFs) extends RootedFs {
    def this(root: String) = this(path(root), fuseJavaFs(path(root)))
    def this(root: Path) = this(root, fuseJavaFs(root))
    def getName = name

    def filterNot(p: String => Boolean) = new Rooted(root, fs filterNot p)
    def mapNode(f: fs.Node =?> fs.Node) = new Rooted(root, fs mapNode f)
  }

  def main(args: Array[String]): Unit = {
    if (runMain isDefinedAt args) runMain(args)
    else Console.err.println(s"Usage: $name $usage")
  }
}
