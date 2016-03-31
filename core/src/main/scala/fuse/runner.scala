package sfs
package fuse

import api._, jio._

object idfs extends FsRunner {
  override def usage = "<from> <to>"
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
  override def usage = "<from> <to>"
  def runMain = { case Array(from, to) =>
    start(
      new Rooted(from) mapNode {
        case File(data) => File(data.get.reverse)
      },
      to
    )
  }
}
object mapfs extends FsRunner {
  override def usage = "<from> <to> <fromExt> <toExt> <command>"
  def runMain = { case Array(from, to, fromExt, toExt, command) =>
    start(
      new Rooted(from).map(
        resolve => {
          case path if path.extension == toExt =>
            resolve(path replaceExtension fromExt).only[Node] mapOnly {
              case File(data) => File(exec(data.get, command).stdout)
            }

          case path if path.extension == fromExt =>
            Metadata

          case path => resolve(path).only[Node] mapOnly {
            case dir: Dir => dir mapOnly {
              case name if name.extension == fromExt =>
                name replaceExtension toExt
            }
          }
        }
      ),
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

  class Rooted(val fs: Filesystem, val store: Store) extends RootedFs {
    def this(root: Path) = this(new jio.JavaFilesystem(root), new jio.JavaStore(root))
    def this(root: String) = this(toPath(root))
    def getName = name

    def filterNot(p: String => Boolean)                  = new Rooted(fs filterNot p, store)
    def mapNode(f: Node =?> Node)                        = new Rooted(fs mapNode f, store)
    def map(f: (Path => Metadata) => (Path => Metadata)) = new Rooted(fs map f, store)
  }

  def main(args: Array[String]): Unit = {
    if (runMain isDefinedAt args) runMain(args)
    else Console.err.println(s"Usage: $name $usage")
  }
}
