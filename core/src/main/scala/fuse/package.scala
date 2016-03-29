package sfs

import net.fusejna.ErrorCodes._
import api._

package object fuse {

  implicit def emptyByteArray: Empty[Array[Byte]] = Empty(Array.empty[Byte])

  val  UTF8 = jio.UTF8

  def alreadyExists()  = -EEXIST
  def doesNotExist()   = -ENOENT
  def eok()            = 0
  def isMac            = scala.util.Properties.isMac
  def isNotValid()     = -EINVAL
  def notImplemented() = -ENOSYS
  def notSupported()   = notImplemented()
  def ioError()        = -EIO
  def notEmpty()       = -ENOTEMPTY
  def tooBig()         = -EFBIG

  def addUnmountHook(fs: FuseFs): Unit =
    scala.sys addShutdownHook ( if (fs.isMounted) fs.unmountTry() )

  // see also: "allow_recursion", "nolocalcaches", "auto_xattr", "sparse"
  def defaultOptions: Vector[String] = isMac match {
    case true => Vector("-o", "direct_io,default_permissions,negative_vncache")
    case _    => Vector("-o", "direct_io,default_permissions")
  }

  type ByteBuffer        = jio.ByteBuffer
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
