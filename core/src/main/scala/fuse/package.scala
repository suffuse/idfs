package sfs

import net.fusejna.ErrorCodes._

package object fuse {

  def alreadyExists()  = -EEXIST
  def doesNotExist()   = -ENOENT
  def eok()            = 0
  def isNotValid()     = -EINVAL
  def notImplemented() = -ENOSYS
  def notSupported()   = notImplemented()

  trait FuseIO {

    def read: Result[Array[Byte]]

    def write(offset: Long, data: Array[Byte]): Result[Unit]

    def lock(): Result[Unit]

    def truncate(size: Long): Result[Unit]
  }

  final case class Permissions(
    ownerRead: Boolean, ownerWrite: Boolean, ownerExecute: Boolean,
    groupRead: Boolean, groupWrite: Boolean, groupExecute: Boolean,
    otherRead: Boolean, otherWrite: Boolean, otherExecute: Boolean
  )
  object Permissions {
    implicit def empty: api.Empty[Permissions] =
      api.Empty(Permissions(false, false, false, false, false, false, false, false, false))

    def fromBits(bits: Long): Permissions = {
      def p(bit: Long) = (bits & bit) != 0
      Permissions(
        p(1 << 8), p(1 << 7), p(1 << 6),
        p(1 << 5), p(1 << 4), p(1 << 3),
        p(1 << 2), p(1 << 1), p(1 << 0)
      )
    }
  }
  // underscore to prevent shadowing
  implicit val _permissions = new api.Key[Permissions]("permissions")

  final class NodeType(`type`: String) extends api.ShowSelf {

    def asFuse = this match {
      case File => FuseFile
      case Dir  => FuseDir
      case Link => FuseLink
    }

    def to_s = `type`
  }
  object NodeType {

    def fromFuse: FuseNodeType => NodeType = {
      case FuseFile => File
      case FuseDir  => Dir
      case FuseLink => Link
    }
  }

  val File = new NodeType("file")
  val Dir  = new NodeType("dir" )
  val Link = new NodeType("link")
  implicit val _nodeType = new api.Key[NodeType]("type of node")

  case class LinkTarget(to: String)
  implicit val _linkTarget = new api.Key[LinkTarget]("link target")

  sealed trait Result[+A] {

    def map[B](f: A => B): Result[B] =
      this match {
        case Success(a) => Success(f(a))
        case x: Error   => x
      }

    def flatMap[B](f: A => Result[B]): Result[B] =
      this match {
        case Success(a) => f(a)
        case x: Error   => x
      }

    def withFilter(f: A => Boolean): Result[A] =
      this match {
        case x @ Success(a) if f(a) => x
        case _                      => InputOutputError
      }

    def orElseUse[AA >: A](z: => AA): Result[AA] =
      this match {
        case x @ Success(_) => x
        case _: Error       => Success(z)
      }

    def orElse[AA >: A](z: => Result[AA]): Result[AA] =
      this match {
        case x @ Success(_) => x
        case _: Error       => z
      }

    def whenSuccessful[AA >: A](z: => Result[AA]): Result[AA] =
      this match {
        case Success(_) => z
        case x: Error   => x
      }

    def ensure(f: A =?> Unit): Result[A] =
      this match {
        case x @ Success(a) if f isDefinedAt a => x
        case Success(_)                        => InputOutputError
        case x: Error                          => x
      }

    def toInt()(implicit ev: A => Int): Int =
      this match {
        case Success(a) => a
        case x: Error   => toErrorCode(x)
      }
  }

  final case class Success[A](value: A) extends Result[A]
  sealed trait Error extends Result[Nothing]

  case object InputOutputError extends Error
  case object AccessDenied     extends Error
  case object TooBig           extends Error
  case object NotEmpty         extends Error
  case object NotImplemented   extends Error
  case object NotValid         extends Error
  case object DoesNotExist     extends Error
  case object AlreadyExists    extends Error
  case object NotSupported     extends Error

  def toErrorCode[A]: Result[A] => Int = {
    case DoesNotExist     => doesNotExist()
    case NotValid         => isNotValid()
    case NotImplemented   => notImplemented()
    case NotEmpty         => -ENOTEMPTY
    case TooBig           => -EFBIG
    case AccessDenied     => -EACCES
    case InputOutputError => -EIO
    case _                => notImplemented()
  }

  import net.fusejna.types.TypeMode.{NodeType => FuseNodeType}

  final val FuseBlockDev = FuseNodeType.BLOCK_DEVICE
  final val FuseDir      = FuseNodeType.DIRECTORY
  final val FuseFifo     = FuseNodeType.FIFO
  final val FuseFile     = FuseNodeType.FILE
  final val FuseLink     = FuseNodeType.SYMBOLIC_LINK
  final val FuseSocket   = FuseNodeType.SOCKET

  final case class Mtime(timestamp: Long)
  final case class Atime(timestamp: Long)
  final case class Size(bytes: Long)

  implicit val _mtime = new api.Key[Mtime]("modification time in ...")
  implicit val _atime = new api.Key[Atime]("access time in ...")
  implicit val _size  = new api.Key[Size]("size in bytes")

  final case class BlockCount(amount: Long)
  implicit val _blockCount = new api.Key[BlockCount]("number of blocks")

  type DirectoryFiller   = net.fusejna.DirectoryFiller
  type FileInfo          = net.fusejna.StructFuseFileInfo.FileInfoWrapper
  type FlockCommand      = net.fusejna.FlockCommand
  type FlockWrapper      = net.fusejna.StructFlock.FlockWrapper
  type FuseContext       = net.fusejna.StructFuseContext
  type FuseException     = net.fusejna.FuseException
  type FuseFilesystem    = net.fusejna.FuseFilesystem
  type IModeInfo         = net.fusejna.types.TypeMode.IModeWrapper
  type ModeInfo          = net.fusejna.types.TypeMode.ModeWrapper
  type FuseNodeType      = net.fusejna.types.TypeMode.NodeType
  type OpenMode          = net.fusejna.StructFuseFileInfo.FileInfoWrapper.OpenMode
  type StatInfo          = net.fusejna.StructStat.StatWrapper
  type StatvfsWrapper    = net.fusejna.StructStatvfs.StatvfsWrapper
  type TimeBufferWrapper = net.fusejna.StructTimeBuffer.TimeBufferWrapper
  type Timespec          = net.fusejna.StructTimespec.ByValue
  type XattrFiller       = net.fusejna.XattrFiller
  type XattrListFiller   = net.fusejna.XattrListFiller

  def addUnmountHook(fs: FuseFs): Unit =
    scala.sys addShutdownHook ( if (fs.isMounted) fs.unmountTry() )

  // see also: "allow_recursion", "nolocalcaches", "auto_xattr", "sparse"
  def defaultOptions = Array("-o", "direct_io,default_permissions")

  implicit class FuseFilesystemOps(val fs: FuseFilesystem) {
    def filter(p: String => Boolean): FuseFs    = new FilteredFs(fs, p)
    def filterNot(p: String => Boolean): FuseFs = new FilteredFs(fs, x => !p(x))
  }
}
