package suffuse
package fs

import jio._

/** Cleaning up the javacentric fuse-jna types.
 */

object Node {
  import net.fusejna.types.TypeMode.NodeType

  final val BlockDev = NodeType.BLOCK_DEVICE
  final val Dir      = NodeType.DIRECTORY
  final val Fifo     = NodeType.FIFO
  final val File     = NodeType.FILE
  final val Link     = NodeType.SYMBOLIC_LINK
  final val Socket   = NodeType.SOCKET
}

abstract class FuseFsFull extends net.fusejna.util.FuseFilesystemAdapterFull with FuseFs {
  def logging(): this.type = doto[this.type](this)(_ log true)
  // Otherwise:
  // [error] /g/idfs/src/main/scala/fs/fusejna.scala:23: class FuseFsFull inherits conflicting members:
  // [error]   method getOptions in class FuseFilesystemAdapterFull of type ()Array[String]  and
  // [error]   method getOptions in trait FuseFs of type ()Array[String]
  override def getOptions() = fs.defaultOptions
}

/** Widening access so we don't have to use inheritance everywhere.
 */

trait FuseFs extends FuseFilesystem {
  def unmountTry(): Unit = (
    if (!isMounted)
      return
    else if (isMac)
      exec("umount", "-f", getMountPoint.getPath) orElse exec("diskutil", "unmount", getMountPoint.getPath)
    else
      exec("fusermount", "-u", getMountPoint.getPath)
  )
  private def doMount(mountPoint: File, blocking: Boolean): this.type = {
    addUnmountHook(this)
    super.mount(mountPoint, blocking)
    this
  }

  def mount(mountPoint: Path): this.type           = doMount(mountPoint.toFile, blocking = false)
  def mountForeground(mountPoint: Path): this.type = doMount(mountPoint.toFile, blocking = true)

  def fuseContext(): FuseContext  = super.getFuseContext
  def getOptions(): Array[String] = fs.defaultOptions

  protected def getUID(): Long = if (isMounted) fuseContext.uid.longValue else 0
  protected def getGID(): Long = if (isMounted) fuseContext.gid.longValue else 0

  protected def populateStat(stat: StatInfo, path: Pathish[_], nodeType: NodeType): Unit = {
    populateMode(stat, path, nodeType)
    stat size   path.size
    stat atime  path.atime
    stat mtime  path.mtime
    stat blocks path.blockCount
    stat nlink 1
    stat uid getUID
    stat gid getGID
  }

  protected def populateMode(mode: IModeInfo, path: Pathish[_], nodeType: NodeType): Unit = {
    val pp = path.permissions
    import pp._
    mode setMode (nodeType,
      ownerRead, ownerWrite, ownerExecute,
      groupRead, groupWrite, groupExecute,
      otherRead, otherWrite, otherExecute
    )
  }
}

/** This makes it easy to modify or extends the behavior of an existing
 *  filesystem instance by overriding a small handful of methods.
 */
abstract class ForwarderFs extends FuseFs {
  protected def underlying: FuseFilesystem

  /** The non-path methods. */
  def afterUnmount(mountPoint: File): Unit = underlying.afterUnmount(mountPoint)
  def beforeMount(mountPoint: File): Unit  = underlying.beforeMount(mountPoint)
  def destroy(): Unit                      = underlying.destroy()
  def getName(): String                    = getClass.getName
  def init(): Unit                         = underlying.init()

  /** Conceptually these are all instance methods of a path. */
  def access(path: String, access: Int): Int                                                   = underlying.access(path, access)
  def bmap(path: String, info: FileInfo): Int                                                  = underlying.bmap(path, info)
  def chmod(path: String, mode: ModeInfo): Int                                                 = underlying.chmod(path, mode)
  def chown(path: String, uid: Long, gid: Long): Int                                           = underlying.chown(path, uid, gid)
  def create(path: String, mode: ModeInfo, info: FileInfo): Int                                = underlying.create(path, mode, info)
  def fgetattr(path: String, stat: StatInfo, info: FileInfo): Int                              = underlying.fgetattr(path, stat, info)
  def flush(path: String, info: FileInfo): Int                                                 = underlying.flush(path, info)
  def fsync(path: String, datasync: Int, info: FileInfo): Int                                  = underlying.fsync(path, datasync, info)
  def fsyncdir(path: String, datasync: Int, info: FileInfo): Int                               = underlying.fsyncdir(path, datasync, info)
  def ftruncate(path: String, offset: Long, info: FileInfo): Int                               = underlying.ftruncate(path, offset, info)
  def getattr(path: String, stat: StatInfo): Int                                               = underlying.getattr(path, stat)
  def getxattr(path: String, xattr: String, filler: XattrFiller, size: Long, pos: Long): Int   = underlying.getxattr(path, xattr, filler, size, pos)
  def link(path: String, target: String): Int                                                  = underlying.link(path, target)
  def listxattr(path: String, filler: XattrListFiller): Int                                    = underlying.listxattr(path, filler)
  def lock(path: String, info: FileInfo, command: FlockCommand, flock: FlockWrapper): Int      = underlying.lock(path, info, command, flock)
  def mkdir(path: String, mode: ModeInfo): Int                                                 = underlying.mkdir(path, mode)
  def mknod(path: String, mode: ModeInfo, dev: Long): Int                                      = underlying.mknod(path, mode, dev)
  def open(path: String, info: FileInfo): Int                                                  = underlying.open(path, info)
  def opendir(path: String, info: FileInfo): Int                                               = underlying.opendir(path, info)
  def read(path: String, buffer: Buf, size: Long, offset: Long, info: FileInfo): Int           = underlying.read(path, buffer, size, offset, info)
  def readdir(path: String, filler: DirectoryFiller): Int                                      = underlying.readdir(path, filler)
  def readlink(path: String, buffer: Buf, size: Long): Int                                     = underlying.readlink(path, buffer, size)
  def release(path: String, info: FileInfo): Int                                               = underlying.release(path, info)
  def releasedir(path: String, info: FileInfo): Int                                            = underlying.releasedir(path, info)
  def removexattr(path: String, xattr: String): Int                                            = underlying.removexattr(path, xattr)
  def rename(path: String, newName: String): Int                                               = underlying.rename(path, newName)
  def rmdir(path: String): Int                                                                 = underlying.rmdir(path)
  def setxattr(path: String, xattr: String, value: Buf, size: Long, flags: Int, pos: Int): Int = underlying.setxattr(path, xattr, value, size, flags, pos)
  def statfs(path: String, wrapper: StatvfsWrapper): Int                                       = underlying.statfs(path, wrapper)
  def symlink(path: String, target: String): Int                                               = underlying.symlink(path, target)
  def truncate(path: String, offset: Long): Int                                                = underlying.truncate(path, offset)
  def unlink(path: String): Int                                                                = underlying.unlink(path)
  def utimens(path: String, wrapper: TimeBufferWrapper): Int                                   = underlying.utimens(path, wrapper)
  def write(path: String, buf: Buf, bufSize: Long, writeOffset: Long, info: FileInfo): Int     = underlying.write(path, buf, bufSize, writeOffset, info)
}

/** Our implementation of the DirFiller interface.
 */
final class DirFiller extends DirectoryFiller {
  private var count = 0
  private val buf = Vector.newBuilder[Path]
  private def add(s: String): Unit = { buf += path(s) ; count += 1 }

  def add(files: jIterable[String]): Boolean = andTrue(files.asScala foreach add)
  def add(files: String*): Boolean           = andTrue(files foreach add)
  lazy val result: Vector[Path]              = try buf.result finally buf.clear()

  override def toString = s"DirFiller($count added so far)"
}
object DirFiller {
  def apply(initial: String*): DirFiller = doto(new DirFiller)(_.add(initial: _*))
}
