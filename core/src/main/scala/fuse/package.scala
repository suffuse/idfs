package sfs

import java.nio.file._
import javax.naming.SizeLimitExceededException
import net.fusejna.ErrorCodes._

package object fuse {

  def tryFuse(body: => Unit): Int = Try(body) fold (_.toErrno, _ => eok)

  def alreadyExists()  = -EEXIST
  def doesNotExist()   = -ENOENT
  def eok()            = 0
  def isMac            = scala.util.Properties.isMac
  def isNotValid()     = -EINVAL
  def notImplemented() = -ENOSYS
  def notSupported()   = notImplemented()

  implicit class ThrowableOps(t: Throwable) {
    println(t)
    println("== CAUSE ==")
    println(t.getCause)
    def toErrno: Int = t match {
      case _: FileAlreadyExistsException    => alreadyExists()
      case _: NoSuchFileException           => doesNotExist()
      case _: IllegalArgumentException      => isNotValid()
      case _: UnsupportedOperationException => notImplemented()
      case _: DirectoryNotEmptyException    => -ENOTEMPTY
      case _: SizeLimitExceededException    => -EFBIG
      case _: AccessDeniedException         => -EACCES
      case _: jio.IOException               => -EIO
      case _                                => -EIO
    }
  }

  def addUnmountHook(fs: FuseFs): Unit =
    scala.sys addShutdownHook ( if (fs.isMounted) fs.unmountTry() )

  // see also: "allow_recursion", "nolocalcaches", "auto_xattr", "sparse"
  def defaultOptions: Vector[String] = Vector("-o", "direct_io,default_permissions")

  implicit class FuseFilesystemOps(val fs: FuseFilesystem) {
    def filter(p: String => Boolean): FuseFs    = new FilteredFs(fs, p)
    def filterNot(p: String => Boolean): FuseFs = new FilteredFs(fs, x => !p(x))
  }

  type FuseCompatibleFs = api.Filesystem {
    type M[A] = Result[A]
    type Path = String
    type Name = String
    type IO   = Array[Byte]
  }

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

  implicit class NodeTypeOps(nodeType: api.attributes.NodeType) {

    import api.attributes._
    def asFuse = nodeType match {
      case File => Node.File
      case Dir  => Node.Dir
      case Link => Node.Link
    }
  }

  // This file exists to experiment with transforming parts of the file system, in this case the Path
  implicit class Wrapped[FS <: api.Filesystem](val underlying: FS) {

    def withPathAsString(
      pathFromString: String => underlying.Path,
        pathToString: underlying.Path => String
    )(implicit F: Functor[underlying.M]) = new api.Filesystem {

      type Path = String

      type M[A] = underlying.M[A]
      type Name = underlying.Name
      type Key  = underlying.Key
      type IO   = underlying.IO

      def resolve(path: Path): Key            = underlying resolve pathFromString(path)
      def metadata(key: Key): M[api.Metadata] = underlying metadata key
      def lookup(key: Key): M[Data]           = underlying lookup key map {
        case underlying.Link(path)    => Link(pathToString(path))
        case underlying.File(io)      => File(io)
        case underlying.Dir(children) => Dir(children)
      }
    }
  }

  type DirectoryFiller   = net.fusejna.DirectoryFiller
  type FileInfo          = net.fusejna.StructFuseFileInfo.FileInfoWrapper
  type FlockCommand      = net.fusejna.FlockCommand
  type FlockWrapper      = net.fusejna.StructFlock.FlockWrapper
  type FuseContext       = net.fusejna.StructFuseContext
  type FuseException     = net.fusejna.FuseException
  type FuseFilesystem    = net.fusejna.FuseFilesystem
  type IModeInfo         = net.fusejna.types.TypeMode.IModeWrapper
  type ModeInfo          = net.fusejna.types.TypeMode.ModeWrapper
  type NodeType          = net.fusejna.types.TypeMode.NodeType
  type OpenMode          = net.fusejna.StructFuseFileInfo.FileInfoWrapper.OpenMode
  type StatInfo          = net.fusejna.StructStat.StatWrapper
  type StatvfsWrapper    = net.fusejna.StructStatvfs.StatvfsWrapper
  type TimeBufferWrapper = net.fusejna.StructTimeBuffer.TimeBufferWrapper
  type Timespec          = net.fusejna.StructTimespec.ByValue
  type XattrFiller       = net.fusejna.XattrFiller
  type XattrListFiller   = net.fusejna.XattrListFiller
}
