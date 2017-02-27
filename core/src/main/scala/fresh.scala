package sfs
package fresh

import fuse._, jio._, api._
import attributes._, attr._

object attr {
  implicit val _nodeData = new api.Key[NodeData]("node data")
  implicit def emptyNodeData = Empty[NodeData](NoNodeData)
}

trait Filesystem {
  def resolve: String => Metadata
  def map(f: Metadata => Metadata): Filesystem   = new Filesystem { def resolve = path => f(resolve(path)) }
  def contraMap(f: String => String): Filesystem = new Filesystem { def resolve = path => resolve(f(path)) }
}

class JavaFilesystem(f: String => jio.Path) extends Filesystem {
  def resolve = path => PathMetadata(f(path)).get
}
object JavaFilesystem {
  def at(root: Path): JavaFilesystem = new JavaFilesystem(p => root append path(p))
}

final case class PathMetadata(path: Path) {
  def get(): Metadata = {
    def md = Metadata(
      Atime(path.atime),
      Mtime(path.mtime),
      UnixPerms(toUnixMask(path.perms)),
      Uid(path.uid)
    )
    path match {
      case p if !p.nofollow.exists => Metadata
      case p if p.isFile           => md set identity[NodeData](FileNode(path.readAllBytes)) set Size(path.size) set BlockCount(path.blockCount)
      case p if p.isDir            => md set identity[NodeData](DirNode(path.ls.map(path => path.filename -> path).toMap))
      case p if p.isLink           => md set identity[NodeData](LinkNode(path.readlink.to_s))
      case _                       => Metadata
    }
  }
}

sealed trait NodeData {
  import net.fusejna.types.TypeMode.NodeType._
  def bits: Long = this match {
    case FileNode(_) => FILE.getBits
    case DirNode(_)  => DIRECTORY.getBits
    case LinkNode(_) => SYMBOLIC_LINK.getBits
    case _           => 0L
  }
}
final case class FileNode(bytes: Array[Byte]) extends NodeData
final case class LinkNode(target: String) extends NodeData
final case class DirNode(list: Map[String, Path]) extends NodeData
final case object NoNodeData extends NodeData

class JavaFuseBridgeFs(fs: JavaFilesystem) extends net.fusejna.util.FuseFilesystemAdapterFull with FuseFs {
  override def getOptions(): Array[String] = options.toArray

  override def getattr(path: String, stat: StatInfo): Int = (fs resolve path) |> (md =>
    md[NodeData] match {
      case NoNodeData => doesNotExist()
      case data       => effect(eok)(populateStat(stat, md, data))
    }
  )
  override def readdir(path: String, filler: DirectoryFiller): Int = (fs resolve path)[NodeData] match {
    case DirNode(childMap) => effect(eok)(childMap.keys foreach (filler add path + "/" + _))
    case _                 => doesNotExist()
  }
  override def readlink(path: String, buf: ByteBuffer, size: Long): Int = (fs resolve path)[NodeData] match {
    case LinkNode(target) => effect(eok)(buf put (target getBytes UTF8))
    case _                => isNotValid()
  }
  override def read(path: String, buf: ByteBuffer, size: Long, offset: Long, info: FileInfo): Int = (fs resolve path)[NodeData] match {
    case FileNode(data) =>
      val totalBytes = if (offset + size > data.length) data.length - offset else size
      effect(totalBytes.toInt)(buf.put(data, offset.toInt, totalBytes.toInt))
    case _ =>
      doesNotExist()
  }
  private def populateStat(stat: StatInfo, md: api.Metadata, data: NodeData): Unit = {
    def applyAttr(attr: Attribute): Unit = attr.value match {
      case Atime(stamp)       => stat atime stamp.inSeconds
      case Mtime(stamp)       => stat mtime stamp.inSeconds
      case BlockCount(amount) => stat blocks amount
      case Uid(value)         => stat uid value
      case UnixPerms(mask)    => stat mode data.bits | mask
      case FileNode(bytes)    => stat size bytes.length ; stat nlink 1
      case LinkNode(target)   => stat size target.length ; stat nlink 1
      case DirNode(kids)      => stat size kids.size + 2 ; stat nlink kids.size + 2
      case _                  =>
    }
    md.attributes foreach applyAttr
    stat gid getGID // XXX huge hassle.
  }
}

abstract class FsRunner {
  def runMain: Array[String] =?> Unit

  def name: String                                = this.getClass.shortName
  def start(fs: FuseFs, mountPoint: String): Unit = fs mountForeground path(mountPoint)

  def main(args: Array[String]): Unit = {
    if (runMain isDefinedAt args) runMain(args)
    else Console.err.println("Bad args.")
  }
}

object idfs extends FsRunner {
  def runMain = { case Array(from, to) =>
    start(new JavaFuseBridgeFs(JavaFilesystem at path(from)), to)
  }
}
