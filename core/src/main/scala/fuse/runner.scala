package sfs
package fuse

import api._, fs._, attributes._

object idfs extends FsRunner {
  override def usage = "<from> <to>"
  def runMain = { case Array(from, to) =>
    new Rooted(from).withLogging mount to
  }
}
object filterfs extends FsRunner {
  override def usage = "<from> <to> <regex>"
  def runMain = { case Array(from, to, regex) =>
    new Rooted(from).reads filterNot (RegexPredicate(regex)) mount to
  }
}
object reversefs extends FsRunner {
  override def usage = "<from> <to>"
  def runMain = { case Array(from, to) =>
    new Rooted(from).reads map DataMap(_.reverse) mount to
  }
}
object mapfs extends FsRunner {
  override def usage = "<from> <to> <fromExt> <toExt> <from-to-command> <to-from-command>\n" +
                       " example: /source /mnt json yaml json2yaml yaml2json"
  def runMain = { case Array(from, to, fromExt, toExt, fromToCommand, toFromCommand) =>
    val e = new transformers.WithExtensionPair(fromExt, toExt)
    new Rooted(from) transform (
      (
        e.asDerivedFilesUsing(exec(_, fromToCommand).stdout) orElse
        e.proxyWritesWith(exec(_, toFromCommand).stdout)
      )
    ) mount to
  }
}

/** Generic SFS runner.
 */
abstract class FsRunner {
  def runMain: Array[String] =?> Unit
  def usage  : String

  def name: String  = this.getClass.shortName

  class Rooted(fs: Filesystem, logging: Boolean) extends FuseFs(fs, name, logging) {

    def this(root: Path)   = this(new jio.JavaFilesystem(root), false)
    def this(root: String) = this(toPath(root))

    def copy(fs: Filesystem = fs, logging: Boolean = logging): Rooted = new Rooted(fs, logging)

    def withLogging() = copy(logging = true)

    object reads {
      import transformers.RemoveWrites

      def filterNot(p: Predicate[Path])            = copy(fs = fs filterNot p andThen RemoveWrites)
      def map(f: Map[Metadata, Metadata])          = copy(fs = fs map f andThen RemoveWrites)
      def transform(transformer: Action ~> Action) = copy(fs = fs transform transformer andThen RemoveWrites)
    }

    def transform(transformer: Action ~> Action) = copy(fs = fs transform transformer)
  }

  def main(args: Array[String]): Unit = {
    if (runMain isDefinedAt args) runMain(args)
    else Console.err.println(s"Usage: $name $usage")
  }
}
