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
    start(new Rooted(from).reads filterNot (_ matches regex), to)
  }
}
object reversefs extends FsRunner {
  override def usage = "<from> <to>"
  def runMain = { case Array(from, to) =>
    start(
      new Rooted(from).reads mapNode {
        case File(data) => File(data.get.reverse)
      },
      to
    )
  }
}
object mapfs extends FsRunner {
  override def usage = "<from> <to> <fromExt> <toExt> <from-to-command> <to-from-command>\n" +
                       " example: /source /mnt json yaml json2yaml yaml2json"
  def runMain = { case Array(from, to, fromExt, toExt, fromToCommand, toFromCommand) =>
    start(
      new Rooted(from).map(
        sourceToUser = resolve => {
          case path if path.extension == toExt =>
            resolve(path replaceExtension fromExt).only[Node] mapOnly {
              case File(data) => File(exec(data.get, fromToCommand).stdout)
            }

          case path if path.extension == fromExt =>
            Metadata

          case path => resolve(path).only[Node] mapOnly {
            case dir: Dir => dir mapOnly {
              case name if name.extension == fromExt =>
                name replaceExtension toExt
            }
          }
        },
        userToSource = {
          case (path, metadata) if path.extension == toExt =>
            val newPath = path replaceExtension fromExt
            val newMetadata = metadata.only[Node] mapOnly {
              case File(data) => File(exec(data.get, toFromCommand).stdout)
            }
            newPath -> newMetadata

          case x => x
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

  class Rooted(val fs: Filesystem) extends RootedFs {
    def this(root: Path) = this(new jio.JavaFilesystem(root))
    def this(root: String) = this(toPath(root))
    def getName = name

    object reads {
      def filterNot(p: String => Boolean)                  = new Rooted(fs.reads filterNot p)
      def mapNode(f: Node =?> Node)                        = new Rooted(fs.reads mapNode f)
      def map(f: (Path => Metadata) => (Path => Metadata)) = new Rooted(fs.reads map f)
    }

    def map(
      sourceToUser: (Path => Metadata) => (Path => Metadata),
      userToSource: ((Path, Metadata)) => (Path, Metadata)
    ) = new Rooted(fs map (sourceToUser, userToSource))
  }

  def main(args: Array[String]): Unit = {
    if (runMain isDefinedAt args) runMain(args)
    else Console.err.println(s"Usage: $name $usage")
  }
}
