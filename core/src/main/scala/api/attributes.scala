package sfs
package api

object attributes {

  // underscore on implicits to prevent shadowing

  final case class UnixPerms(mask: Long) {
    import UnixPerms._

    def bits: Set[Long] = BitsSet filter (bit => (bit & mask) != 0)

    override def toString = permString(mask)
  }
  object UnixPerms {
    lazy val Bits = Vector[Long](
      1 << 8,
      1 << 7,
      1 << 6,
      1 << 5,
      1 << 4,
      1 << 3,
      1 << 2,
      1 << 1,
      1 << 0
    )
    private lazy val BitsSet: Set[Long]    = Bits.toSet
    private lazy val Letters: Vector[Char] = "rwxrwxrwx".toVector

    private def permString(mask: Long): String =
      ( for ((perm, ch) <- Bits zip Letters) yield if ((mask & perm) == 0) '-' else ch ) mkString ""
  }
  implicit val _unixPerms = new Key[UnixPerms]("unix permissions")

  final class NodeType(`type`: String) extends api.ShowSelf {
    def to_s = `type`
  }
  val File = new NodeType("file")
  val Dir  = new NodeType("dir" )
  val Link = new NodeType("link")
  implicit val _nodeType = new api.Key[NodeType]("type of node")

  final case class Mtime(timestamp: Long)
  implicit val _mtime = new api.Key[Mtime]("modification time in ...")

  final case class Atime(timestamp: Long)
  implicit val _atime = new api.Key[Atime]("access time in ...")

  final case class Size(bytes: Long)
  implicit val _size = new api.Key[Size]("size in bytes")

  final case class Uid(value: Int)
  implicit val _uid = new api.Key[Uid]("uid ...")

  final case class BlockCount(amount: Long)
  implicit val _blockCount = new api.Key[BlockCount]("number of blocks")
}
