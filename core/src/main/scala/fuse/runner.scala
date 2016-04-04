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
    start(new Rooted(from).reads filterNot (_.toString matches regex), to)
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
    val e = new transformers.WithExtensionPair(fromExt, toExt)
    start(
      new Rooted(from) transform (
        (
          e.asDerivedFilesUsing(exec(_, fromToCommand).stdout) orElse
          e.proxyWritesWith(exec(_, toFromCommand).stdout)
        )
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
      import transformers.RemoveWrites

      def filterNot(p: Path => Boolean)            = new Rooted(fs filterNot p andThen RemoveWrites)
      def mapNode(f: Node =?> Node)                = new Rooted(fs mapNode f andThen RemoveWrites)
      def map(f: Metadata => Metadata)             = new Rooted(fs map f andThen RemoveWrites)
      def transform(transformer: Action ~> Action) = new Rooted(fs transform transformer andThen RemoveWrites)
    }

    def transform(transformer: Action ~> Action) = new Rooted(fs transform transformer)
  }

  def main(args: Array[String]): Unit = {
    if (runMain isDefinedAt args) runMain(args)
    else Console.err.println(s"Usage: $name $usage")
  }
}
