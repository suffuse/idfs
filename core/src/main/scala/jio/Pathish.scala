package sfs
package jio

import java.nio.{ file => jnf }
import jnf.{ attribute => jnfa }
import jnf.Files
import jnfa.PosixFilePermissions.asFileAttribute
import api._

trait Pathish[Rep] {
  def path: Path
  def asRep(p: Path): Rep

  def /(name: String): Rep      = asRep(path resolve name)

  def ls: Vector[Rep]           = if (path.nofollow.isDirectory) withDirStream(path)(_.toVector map asRep) else Vector()
  def mkdir(bits: Long): Rep    = asRep(path createDirectory asFileAttribute(bitsAsPermissions(bits)))
  def mkfile(bits: Long): Rep   = asRep(path createFile asFileAttribute(bitsAsPermissions(bits)))
  def mklink(target: Path): Rep = asRep(path createSymbolicLink target)
  def readlink: Rep             = asRep(path.readSymbolicLink)

  def metadata: Metadata = {
    import api.attributes._
    val metadata =
      Metadata(
        Atime(atime),
        Mtime(mtime),
        UnixPerms(toUnixMask(perms)),
        Uid(uid)
      )

         if (isFile) metadata set File set Size(path.size) set BlockCount(blockCount)
    else if (isDir ) metadata set Dir
    else if (isLink) metadata set Link
    else metadata
  }
  def uid: Int                         = (UidMethod invoke owner).asInstanceOf[Int]
  def gid: Int                         = 0 // OMG what a hassle.
  def group: GroupPrincipal            = posixAttributes.group
  def inum: Object                     = basicAttributes.fileKey
  def owner: UserPrincipal             = posixAttributes.owner
  def perms: jSet[PosixFilePermission] = posixAttributes.permissions

  def atime: FileTime = basicAttributes.lastAccessTime
  def ctime: FileTime = basicAttributes.creationTime
  def mtime: FileTime = basicAttributes.lastModifiedTime

  def isDir: Boolean   = path.nofollow.isDirectory
  def isFile: Boolean  = path.nofollow.isRegularFile
  def isLink: Boolean  = path.isSymbolicLink
  def isOther: Boolean = basicAttributes.isOther

  private def withDirStream[A](dir: Path)(code: jStream[Path] => A): A =
    (Files list dir) |> (str => try code(str) finally str.close())

  def attributes[A <: BasicFileAttributes : CTag](): Try[A] = Try(path.nofollow readAttributes classOf[A])
  def posixAttributes: PosixFileAttributes                  = attributes[PosixFileAttributes] | ??? // FIXME
  def basicAttributes: BasicFileAttributes                  = attributes[BasicFileAttributes] | ??? // FIXME

  def blockCount: Long     = (path.size + blockSize - 1) / blockSize
  def blockSize: Long      = 512 // FIXME
  def depth: Int           = path.getNameCount
  def filename: String     = path.getFileName.to_s
  def mediaType: MediaType = MediaType(exec("file", "--brief", "--mime", "--dereference", to_s).stdout mkString "\n")
  def moveTo(target: Path) = path.nofollow move target
  def to_s: String         = path.toString
}
