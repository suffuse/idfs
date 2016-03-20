package sfs
package api

object attributes {

  final case class Permissions(
    ownerRead: Boolean, ownerWrite: Boolean, ownerExecute: Boolean,
    groupRead: Boolean, groupWrite: Boolean, groupExecute: Boolean,
    otherRead: Boolean, otherWrite: Boolean, otherExecute: Boolean
  )
  object Permissions {
    implicit def empty: api.Empty[Permissions] =
      api.Empty(Permissions(false, false, false, false, false, false, false, false, false))
  }
  // underscore to prevent shadowing
  implicit val _permissions = new api.Key[Permissions]("permissions")

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
