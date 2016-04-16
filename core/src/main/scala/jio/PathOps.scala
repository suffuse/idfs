package sfs
package jio

import api._
import java.nio.{ channels => jnc }
import java.nio.{ file => jnf }
import jnf.{ attribute => jnfa }
import jnfa.PosixFilePermissions.asFileAttribute
import jnf.StandardCopyOption.REPLACE_EXISTING
import java.util.concurrent.TimeUnit
import javax.naming.SizeLimitExceededException

class PathOpsInstance(path: Path) {
  def /(name: String): Path      = path resolve name

  def ls: Vector[Path]           = if (path.nofollow.isDirectory) path.withDirStream(_.toVector) else Vector()
  def mkdir(bits: Long): Path    = path createDirectory asFileAttribute(toJavaPermissions(bits))
  def mkfile(bits: Long): Path   = path createFile asFileAttribute(toJavaPermissions(bits))
  def mklink(target: Path): Path = path createSymbolicLink target
  def readlink: Path             = path.readSymbolicLink
  def lastSegment: Name          = Option(path.getFileName) map (_.to_s) getOrElse ""

  def uid: Int                         = path.nofollow.getAttribute("unix:uid").asInstanceOf[Int]
  def uid_=(id: Int)                   = path.nofollow.setAttribute("unix:uid", id)
  def gid: Int                         = path.nofollow.getAttribute("unix:gid").asInstanceOf[Int]
  def gid_=(id: Int)                   = path.nofollow.setAttribute("unix:gid", id)
  def group: GroupPrincipal            = posixAttributes.group
  def inum: Object                     = basicAttributes.fileKey
  def owner: UserPrincipal             = posixAttributes.owner
  def perms: jSet[PosixFilePermission] = posixAttributes.permissions
  def nlink: Int                       = path.nofollow.getAttribute("unix:nlink").asInstanceOf[Int]

  def birth: FileTime = basicAttributes.creationTime
  def atime: FileTime = basicAttributes.lastAccessTime
  def ctime: FileTime = path.nofollow.getAttribute("unix:ctime").asInstanceOf[FileTime]
  def mtime: FileTime = basicAttributes.lastModifiedTime

  def isDir: Boolean   = path.nofollow.isDirectory
  def isFile: Boolean  = path.nofollow.isRegularFile
  def isLink: Boolean  = path.isSymbolicLink
  def isOther: Boolean = basicAttributes.isOther

  def attributes[A <: BasicFileAttributes : CTag](): Try[A] = Try(path.nofollow readAttributes classOf[A])
  def posixAttributes: PosixFileAttributes                  = attributes[PosixFileAttributes] | ??? // FIXME
  def basicAttributes: BasicFileAttributes                  = attributes[BasicFileAttributes] | ??? // FIXME

  def blockCount: Long     = (path.size + blockSize - 1) / blockSize
  def blockSize: Long      = 512 // FIXME
  def depth: Int           = path.getNameCount
  def extension: String    = filename.extension
  def filename: String     = lastSegment
  def mediaType: MediaType = MediaType(exec("file", "--brief", "--mime", "--dereference", to_s).stdout mkString "\n")
  def moveTo(target: Path) = path.nofollow move (target, REPLACE_EXISTING)
  def to_s: String         = path.toString

  def append(other: Path) = jio.path(path.to_s + other.to_s)

  def replaceExtension(newExtension: String): Path = path.resolveSibling(filename replaceExtension newExtension)

  def truncate(size: Long): Unit = withWriteChannel {
    case c if c.size > size => c truncate size
    case c if c.size < size => c appendNullBytes (at = c.size, amount = (size - c.size).toInt)
                               if (c.size < size) throw new SizeLimitExceededException
    case _                  => // sizes are equal
  }

  private def withWriteChannel[A](code: jnc.FileChannel => A): A = {
    val channel = jnc.FileChannel.open(path, jnf.StandardOpenOption.WRITE)
    try code(channel) finally channel.close()
  }
}
