package sfs
package fuse

import jio._
import java.util.concurrent.TimeUnit.SECONDS

/** Forwarding filesystem which only passes through paths which match the filter.
 */
class FilteredFs(val underlying: FuseFilesystem, cond: String => Boolean) extends ForwarderFs {
  class Filter(filler: DirectoryFiller) extends DirectoryFiller {
    def add(files: jIterable[String]): Boolean = filler add (files.asScala filter cond).asJava
    def add(files: String*): Boolean           = filler add (files filter cond).asJava
  }
  override def readdir(path: String, df: DirectoryFiller): Int =
    underlying.readdir(path, new Filter(df))
}

abstract class FuseFsFull extends net.fusejna.FuseFilesystem with FuseFs {
  def logging(): this.type = doto[this.type](this)(_ log true)
}

class RootedFsClass(val name: String, val root: Path, val fs: FuseCompatibleFs) extends RootedFs {
  def getName = name
}

trait RootedFs extends FuseFsFull {
  def root: Path
  def rootFile = root.toFile

  // dependent types force us to have a single fs (a `val`)
  val fs: FuseCompatibleFs

  protected def fuseContext: FuseContext
  protected def resolvePath(p: String): Path = path(s"$root$p")
  protected def resolveFile(p: String): File = if (p == "/") rootFile else rootFile / (p stripPrefix "/")

  def read(path: String, buf: ByteBuffer, size: Long, offset: Long, info: FileInfo): Int = {
    for {
      key           <- fs resolve path
      fs.File(data) <- fs lookup key
      totalBytes    =  if (offset + size > data.length) data.length - offset else size
      _             =  buf.put(data, offset.toInt, totalBytes.toInt)
    } yield totalBytes.toInt
  }.toInt

  def write(path: String, buf: ByteBuffer, size: Long, offset: Long, info: FileInfo): Int = {
    def impl(): Unit = {
      val arr = new Array[Byte](size.toInt)
      buf get arr
      val f = resolveFile(path)
      f appending (_ write arr)
    }
    Try(impl) fold (_ => -1, _ => size.toInt)
  }

  def lock(path: String, info: FileInfo, command: FlockCommand, flock: FlockWrapper): Int =
    tryFuse { resolvePath(path).tryLock() }

  def readdir(path: String, filler: DirectoryFiller): Int = {
    for {
      key              <- fs resolve path
      fs.Dir(children) <- fs lookup key ensure fs.isDir orElseUse empty[fs.Dir]
      _                =  children.keys foreach (child => filler add (path + "/" + child))
    } yield eok
  }.toInt

  def readlink(path: String, buf: ByteBuffer, size: Long): Int = {
    for {
      key             <- fs resolve path
      fs.Link(target) <- fs lookup key ensure fs.isLink orElse NotValid
      _               =  buf put (target getBytes UTF8)
    } yield eok
  }.toInt

  def create(path: String, mode: ModeInfo, info: FileInfo): Int = {
    import Node._
    val p = resolvePath(path)
    mode.`type`() match {
      case Dir             => tryFuse(p mkdir mode.mode)
      case File            => tryFuse(p mkfile mode.mode)
      case Fifo | Socket   => notSupported()
      case BlockDev | Link => notSupported()
    }
  }

  def mkdir(path: String, mode: ModeInfo): Int =
    resolvePath(path) match {
      case f if f.nofollow.exists => alreadyExists()
      case f                      => effect(eok)(f.mkdir(mode.mode))
    }

  def getattr(path: String, stat: StatInfo): Int = {
    for {
      key      <- fs resolve path
      metadata <- fs metadata key
      _        <- populateStat(stat, metadata)
    } yield eok
  }.toInt

  def rename(from: String, to: String): Int =
    tryFuse { resolvePath(from) moveTo resolvePath(to) }

  def rmdir(path: String): Int =
    tryFuse {
      resolvePath(path) match {
        case d if d.nofollow.isDirectory => effect(eok)(d.delete())
        case _                           => doesNotExist()
      }
    }

  def unlink(path: String): Int =
    resolvePath(path) match {
      case f if f.nofollow.exists => effect(eok)(f.delete())
      case _                      => doesNotExist()
    }

  // p.follow, because symbolic links don't have meaningful permissions
  // chmod calls to a link are applied to the link target.
  def chmod(path: String, mode: ModeInfo): Int =
    resolvePath(path) match {
      case p if p.follow.exists => effect(eok)(p.setPermissions(mode.mode))
      case _                    => doesNotExist()
    }

  def symlink(target: String, linkName: String): Int =
    tryFuse { resolvePath("/" + linkName) mklink path(target) }

  def link(from: String, to: String): Int =
    notSupported()

  def truncate(path: String, size: Long): Int =
    tryFuse { effect(eok)(resolvePath(path) truncate size) }

  def utimens(path: String, wrapper: TimeBufferWrapper) =
    tryFuse(resolvePath(path) setLastModifiedTime wrapper.mod_nsec)

  protected def pathBytes(path: Path): Array[Byte] = path.readAllBytes

  private def populateStat(stat: StatInfo, metadata: api.Metadata): Result[Unit] = {
    import api.attributes._
    for {
      nodeType    <- metadata fold[NodeType] (ifValue = Success(_), orElse = DoesNotExist)
      permissions =  metadata[Permissions]
    } yield {

      metadata foreach {
        case Size(bytes)        => stat size   bytes
        case Atime(timestamp)   => stat atime  timestamp
        case Mtime(timestamp)   => stat mtime  timestamp
        case BlockCount(amount) => stat blocks amount
        case Uid(value)         => stat uid    value
      }

      import permissions._

      stat setMode (nodeType.asFuse,
        ownerRead, ownerWrite, ownerExecute,
        groupRead, groupWrite, groupExecute,
        otherRead, otherWrite, otherExecute
      )

      stat nlink  1
      stat gid    getGID // XXX huge hassle.
    }
  }

  // comments taken from https://www.cs.hmc.edu/~geoff/classes/hmc.cs135.201109/homework/fuse/fuse_doc.html#function-purposes

  def afterUnmount(mountPoint: java.io.File): Unit = {}
  def beforeMount(mountPoint: java.io.File): Unit = {}
  //  Called when the filesystem exits.
  def destroy(): Unit = {}

  //  This is the same as the access(2) system call. It returns -ENOENT if the path doesn't
  //  exist, -EACCESS if the requested permission isn't available, or 0 for success. Note that
  //  it can be called on files, directories, or any other object that appears in the filesystem.
  //  This call is not required but is highly recommended.
  def access(path: String, access: Int): Int = notImplemented()

  //  This function is similar to bmap(9). If the filesystem is backed by a block device, it
  //  converts blockno from a file-relative block number to a device-relative block.
  def bmap(path: String, info: FileInfo): Int = eok

  //  Change the given object's owner and group to the provided values. See chown(2) for details.
  //  NOTE: FUSE doesn't deal particularly well with file ownership, since it usually runs as an
  //  unprivileged user and this call is restricted to the superuser. It's often easier to pretend
  //  that all files are owned by the user who mounted the filesystem, and to skip implementing
  //  this function.
  def chown(path: String, uid: Long, gid: Long): Int = eok

  //  As getattr, but called when fgetattr(2) is invoked by the user program.
  def fgetattr(path: String, stat: StatInfo, info: FileInfo): Int = getattr(path, stat)

  //  Called on each close so that the filesystem has a chance to report delayed errors. Important:
  //  there may be more than one flush call for each open. Note: There is no guarantee that flush will
  //  ever be called at all!
  def flush(path: String, info: FileInfo): Int = eok

  //  Flush any dirty information about the file to disk. If isdatasync is nonzero, only data, not
  //  metadata, needs to be flushed. When this call returns, all file data should be on stable storage.
  //  Many filesystems leave this call unimplemented, although technically that's a Bad Thing since it
  //  risks losing data. If you store your filesystem inside a plain file on another filesystem, you can
  //  implement this by calling fsync(2) on that file, which will flush too much data (slowing performance)
  //  but achieve the desired guarantee.
  def fsync(path: String, datasync: Int, info: FileInfo): Int = eok

  //  Like fsync, but for directories.
  def fsyncdir(path: String, datasync: Int, info: FileInfo): Int = eok

  //  As truncate, but called when ftruncate(2) is called by the user program.
  def ftruncate(path: String, size: Long, info: FileInfo): Int = truncate(path, size)

  //  Read an extended attribute. See getxattr(2). This should be implemented only if HAVE_SETXATTR is true.
  def getxattr(path: String, xattr: String, filler: net.fusejna.XattrFiller, size: Long, position: Long): Int = notImplemented()

  //  Initialize the filesystem. This function can often be left unimplemented, but it can be a handy way to
  //  perform one-time setup such as allocating variable-sized data structures or initializing a new
  //  filesystem. The fuse_conn_info structure gives information about what features are supported by FUSE,
  //  and can be used to request certain capabilities (see below for more information). The return value of
  //  this function is available to all file operations in the private_data field of fuse_context. It is
  //  also passed as a parameter to the destroy() method. (Note: see the warning under Other Options below,
  //  regarding relative pathnames.)
  //
  //  EE - The jna implementation does not give us the info structure apparently
  def init(): Unit = {}

  //  List the names of all extended attributes. See listxattr(2). This should be implemented only if
  //  HAVE_SETXATTR is true.
  def listxattr(path: String, filler: net.fusejna.XattrListFiller): Int = notImplemented()

  // Make a special (device) file, FIFO, or socket. See mknod(2) for details. This function is rarely needed,
  //  since it's uncommon to make these objects inside special-purpose filesystems.
  def mknod(path: String, mode: ModeInfo, dev: Long): Int = create(path, mode, null)

  //  Open a file. If you aren't using file handles, this function should just check for existence and
  //  permissions and return either success or an error code. If you use file handles, you should also
  //  allocate any necessary structures and set fi->fh. In addition, fi has some other fields that an
  //  advanced filesystem might find useful; see the structure definition in fuse_common.h for very brief
  //  commentary.
  def open(path: String, info: FileInfo): Int = eok

  //  Open a directory for reading.
  def opendir(path: String, info: FileInfo): Int = eok

  //  This is the only FUSE function that doesn't have a directly corresponding system call, although close(2)
  //  is related. Release is called when FUSE is completely done with a file; at that point, you can free up
  //  any temporarily allocated data structures. The IBM document claims that there is exactly one release per
  //  open, but I don't know if that is true.
  def release(path: String, info: FileInfo): Int = eok

  //  This is like release, except for directories.
  def releasedir(path: String, info: FileInfo): Int = eok

  def removexattr(path: String, xattr: String): Int = notImplemented()

  //  Set an extended attribute. See setxattr(2). This should be implemented only if HAVE_SETXATTR is true.
  def setxattr(path: String, xattr: String, buf: ByteBuffer, size: Long, flags: Int, position: Int): Int = notImplemented()

  //  Return statistics about the filesystem. See statvfs(2) for a description of the structure contents.
  //  Usually, you can ignore the path. Not required, but handy for read/write filesystems since this is how
  //  programs like df determine the free space.
  def statfs(path: String, statfs: StatvfsWrapper): Int = eok
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

  def fillDir(df: DirectoryFiller)(xs: Traversable[Any]): Unit = xs foreach (df add "" + _)

  def mount(mountPoint: Path): this.type           = doMount(mountPoint.toFile, blocking = false)
  def mountForeground(mountPoint: Path): this.type = doMount(mountPoint.toFile, blocking = true)

  def fuseContext(): FuseContext  = super.getFuseContext
  def getOptions(): Array[String] = options.toArray

  def options: Vector[String] = fuse.defaultOptions
  def getUID(): Long = if (isMounted) fuseContext.uid.longValue else 0
  def getGID(): Long = if (isMounted) fuseContext.gid.longValue else 0
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
  def getName(): String                    = getClass.shortName
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
