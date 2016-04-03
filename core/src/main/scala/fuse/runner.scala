package sfs
package fuse

import api._, jio._, attributes._

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
      new Rooted(from) transform new Transformer {
        def transform[A] = {

          case Resolve(path) if path.extension == toExt =>
            for { metadata <- Resolve(path replaceExtension fromExt) }
            yield metadata[Node] match {
              case File(data) =>
                val newData = exec(data.get, fromToCommand).stdout
                metadata set File(newData) set Size(newData.size)
              case x => metadata
            }

          case action @ Resolve(path) if path.extension == fromExt =>
            InstantResult(empty[Metadata])

          case action: Resolve =>
            action.map(_.only[Node] map {
              case dir: Dir => dir mapOnly {
                case name if name.extension == fromExt =>
                  name replaceExtension toExt
              }
              case x => x // some compiler bug has a problem with `mapOnly`
            })

          case Write(path, data) if path.extension == toExt =>
            Write(path replaceExtension fromExt, exec(data, toFromCommand).stdout)

          case Move(path, to) if to.extension == toExt =>
            for {
              metadata <- Resolve(path)
              data     =  metadata[Node] match {
                            case File(data) => data.get
                            case _          => empty[Data]
                          }
              _        <- Write(path, exec(data, toFromCommand).stdout)
              _        <- Move(path, to replaceExtension fromExt)
            } yield ()
        }
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

    object reads {
      def filterNot(p: String => Boolean)          = new Rooted(fs.reads filterNot p)
      def mapNode(f: Node =?> Node)                = new Rooted(fs.reads mapNode f)
      def map(f: Metadata => Metadata)             = new Rooted(fs.reads map f)
      def transform(transformer: Action ~> Action) = new Rooted(fs.reads transform transformer)
    }

    def transform(transformer: Action ~> Action) = new Rooted(fs transform transformer)
  }

  def main(args: Array[String]): Unit = {
    if (runMain isDefinedAt args) runMain(args)
    else Console.err.println(s"Usage: $name $usage")
  }
}
