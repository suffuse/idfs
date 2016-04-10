package sfs
package api

import scala.reflect.ClassTag

object attributes {
  // underscore on implicits to prevent shadowing

  final case class UnixPerms(mask: Long) {
    override def toString = UnixPerms permString mask
    def noWrites: UnixPerms =
      UnixPerms(mask & (UnixPerms toMask "r-xr-xr-x"))
  }
  object UnixPerms {

    def apply(perms: String): UnixPerms = UnixPerms(toMask(perms))

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

  final case class Mtime(timestamp: FileTime) extends FileTimeBased[Mtime](Mtime)
  implicit val _mtime = new api.Key[Mtime]("modification time")

  final case class Atime(timestamp: FileTime) extends FileTimeBased[Atime](Atime)
  implicit val _atime = new api.Key[Atime]("access time")

  final case class Ctime(timestamp: FileTime) extends FileTimeBased[Ctime](Ctime)
  implicit val _ctime = new api.Key[Ctime]("node or file change time")

  final case class Birth(timestamp: FileTime) extends FileTimeBased[Birth](Birth)
  implicit val _birth = new api.Key[Birth]("creation time")

  final case class Size(bytes: Long)
  implicit val _size = new api.Key[Size]("size in bytes")

  final case class Uid(value: Int)
  implicit val _uid = new api.Key[Uid]("user id")

  final case class Gid(value: Int)
  implicit val _gid = new api.Key[Gid]("group id")

  final case class BlockCount(amount: Long)
  implicit val _blockCount = new api.Key[BlockCount]("number of blocks")

  final case class Nlink(count: Int)
  implicit val _nlink = new api.Key[Nlink]("number of links")

  abstract class FileTimeBased[This <: FileTimeBased[This] : ClassTag](val create: FileTime => This) {
    _: This  => // helps to prevent us from making copy-paste mistakes

    def timestamp: FileTime
    def +(amount: Duration): This = create(timestamp + amount)

    override def hashCode = timestamp.toMillis.##
    override def equals(that: Any) = that match {
      case other: This  => timestamp.toMillis == other.timestamp.toMillis
      case _            => super.equals(that)
    }
  }
}
