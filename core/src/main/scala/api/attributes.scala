package sfs
package api

object attributes {
  // underscore on implicits to prevent shadowing

  abstract class FileTimeBased[This](create: FileTime => This) {
    def timestamp: FileTime
    def +(amount: Duration): This = create(timestamp + amount)
  }

  final case class UnixPerms(mask: Long) {
    override def toString = UnixPerms permString mask
    def noWrites: UnixPerms =
      UnixPerms(mask & (UnixPerms toMask "r-xr-xr-x"))
  }
  object UnixPerms {

    def toBitSet(mask: Long): Set[Long] = BitsSet filter (bit => (bit & mask) != 0)

    val Bits = Vector[Long](
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
    private val BitsSet: Set[Long]    = Bits.toSet
    private val Letters: Vector[Char] = "rwxrwxrwx".toVector

    private def permString(mask: Long): String =
      ( for ((perm, ch) <- Bits zip Letters) yield if ((mask & perm) == 0) '-' else ch ) mkString ""

    private def toMask(perms: String): Long =
      ( for ((perm, ch) <- Bits zip perms.toSeq ; if ch != '-') yield perm ).foldLeft(0L)(_ | _)
  }
  implicit val _unixPerms = new Key[UnixPerms]("unix permissions")

  final case class Mtime(timestamp: FileTime) extends FileTimeBased[Mtime](x => Mtime(x)) {
    override def hashCode = timestamp.toMillis.##
    override def equals(that: Any) = that match {
      case Mtime(other) => timestamp.toMillis == other.toMillis
      case _            => super.equals(that)
    }
  }
  implicit val _mtime = new api.Key[Mtime]("modification time in ...")

  final case class Atime(timestamp: FileTime)
  implicit val _atime = new api.Key[Atime]("access time in ...")

  final case class Size(bytes: Long)
  implicit val _size = new api.Key[Size]("size in bytes")

  final case class Uid(value: Int)
  implicit val _uid = new api.Key[Uid]("uid ...")

  final case class BlockCount(amount: Long)
  implicit val _blockCount = new api.Key[BlockCount]("number of blocks")

  final case class Nlink(count: Int)
  implicit val _nlink = new api.Key[Nlink]("number of links")

}
