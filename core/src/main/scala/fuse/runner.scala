package sfs
package fuse

import api._, fs._, attributes._

object idfs extends FsRunner(logging = true) {
  def usage  = "<from> <to>"
  def create = { case Array(from, to) =>
    prepare(Rooted(at = from), mountPoint = to)
  }
}
object filterfs extends FsRunner {
  def usage  = "<from> <to> <regex>"
  def create = { case Array(from, to, regex) =>
    prepare(Rooted(at = from).reads filterNot (_.to_s matches regex), mountPoint = to)
  }
}
object reversefs extends FsRunner {
  def usage  = "<from> <to>"
  def create = { case Array(from, to) =>
    prepare(Rooted(at = from).reads map (_ mapDataOfFiles (_.reverse)), mountPoint = to)
  }
}
object mapfs extends FsRunner {
  def usage  = "<from> <to> <fromExt> <toExt> <from-to-command> <to-from-command>\n" +
               " example: /source /mnt json yaml json2yaml yaml2json"
  def create = { case Array(from, to, fromExt, toExt, fromToCommand, toFromCommand) =>
    val e = new transformers.WithExtensionPair(fromExt, toExt)
    prepare(
      Rooted(at = from) transform {
        e.asDerivedFilesUsing(exec(_, fromToCommand).stdout) orElse
        e.proxyWritesWith(exec(_, toFromCommand).stdout)
      },
      mountPoint = to
    )
  }
}
object concatfs extends FsRunner {
  def usage   = "<to> <from1> [... <fromN>]"
  def create = { case Array(to, from, others @ _*) =>
    prepare(
      others.foldLeft(Rooted(at = from).reads) { (fs, path) =>
        (fs concat Rooted(at = path)).reads
      },
      mountPoint = to
    )
  }

}

/** Generic SFS runner.
 */
abstract class FsRunner(logging: Boolean = false) {

  def main(args: Array[String]): Unit = {
    if (create isDefinedAt args) create(args).mount()
    else Console.err.println(s"Usage: $name $usage")
  }

  protected def create: Array[String] =?> PreparedFilesystem
  protected def usage : String

  def apply(args: String*) = {
    val argArray = args.toArray
    if (create isDefinedAt argArray) create(argArray)
    else sys error (s"$name $usage")
  }

  def name: String  = this.getClass.shortName

  protected def prepare(fs: Filesystem, mountPoint: String) = new PreparedFilesystem(fs, mountPoint)

  protected class Rooted(fs: Filesystem) extends Filesystem {
    def this(at: String) = this(new jio.JavaFilesystem(toPath(at)))

    def apply[A](action: Action[A]) = fs apply action
  }
  protected object Rooted {
    def apply(at: String): Filesystem = new Rooted(at)
  }

  class Mountable(val fs: Filesystem, logging: Boolean = false) extends FuseFs(fs, name, logging)

  class PreparedFilesystem(fs: Filesystem, mountPoint: String, logging: Boolean = logging) {
    private lazy val mountable = new Mountable(fs, logging)
    def mount()                = mountable mount mountPoint
    def mountBackground()      = mountable mountBackground mountPoint

    def logging    = new PreparedFilesystem(fs, mountPoint, logging = true)
    def notLogging = new PreparedFilesystem(fs, mountPoint, logging = false)
  }
}
