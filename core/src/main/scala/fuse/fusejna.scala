package sfs
package fuse

import api._, fs._, attributes._

class FuseImplementation(
  val fs  : Filesystem,
  name    : String,
  options : Vector[String] = fuse.defaultOptions
) extends FuseFilesystem {

  protected def getName    = name
  protected def getOptions = options.toArray

  def logging(): this.type = doto[this.type](this)(_ log true)

  def getattr(path: String, stat: StatInfo): Int =
    resolve(path) |> { metadata =>
      metadata[Node] match {
        case NoNode => doesNotExist
        case node   => effect(eok)(populateStat(stat, node, metadata))
      }
    }

  def fgetattr(path: String, stat: StatInfo, info: FileInfo): Int = getattr(path, stat)

  def read(path: String, buf: ByteBuffer, size: Long, offset: Long, info: FileInfo): Int =
    resolve(path)[Node] match {
      case File(data) =>
        val totalBytes = if (offset + size > data.get.length) data.get.length - offset else size
        buf.put(data.get, offset.toInt, totalBytes.toInt)
        totalBytes.toInt

      case NoNode    => doesNotExist
      case _         => isNotValid
    }

  def readdir(path: String, filler: DirectoryFiller): Int =
    resolve(path)[Node] match {
      case Dir(kids) => effect(eok)(kids foreach (filler add path + "/" + _))
      case NoNode    => doesNotExist
      case _         => eok
    }

  def readlink(path: String, buf: ByteBuffer, size: Long): Int =
    resolve(path)[Node] match {
      case Link(target) =>
        buf put (target getBytes UTF8)
        eok

      case NoNode => doesNotExist
      case _      => isNotValid
    }

  def create(path: String, mode: ModeInfo, info: FileInfo): Int = {
    import Node._
    mode.`type`() match {
      case Dir             => effect(eok)(createDir(path, UnixPerms(mode.mode)))
      case File            => effect(eok)(createFile(path, UnixPerms(mode.mode)))
      case Fifo | Socket   => notSupported
      case BlockDev | Link => notSupported
    }
  }

  def mknod(path: String, mode: ModeInfo, dev: Long): Int = create(path, mode, null)

  def mkdir(path: String, mode: ModeInfo): Int =
    resolve(path)[Node] match {
      case NoNode => effect(eok)(createDir(path, UnixPerms(mode.mode)))
      case _      => alreadyExists
    }

  def symlink(target: String, linkName: String): Int =
    resolve(linkName)[Node] match {
      case NoNode => effect(eok)(createLink(linkName, target))
      case _      => alreadyExists
    }

  def link(from: String, to: String): Int =
    notSupported

  def write(path: String, buf: ByteBuffer, size: Long, offset: Long, info: FileInfo): Int = {
    // note that offset is ignored
    def data = {
      val arr = new Array[Byte](size.toInt)
      buf get arr
      arr
    }
    effect(size.toInt)(write(path, data))
  }

  def rename(from: String, to: String): Int =
    (resolve(from)[Node], resolve(to)[Node]) match {
      case (NoNode, _)                     => doesNotExist
      case (_, Dir(kids)) if kids.nonEmpty => notEmpty
      case (_, _)                          => effect(eok)(move(from, to))
    }

  def rmdir(path: String): Int =
    resolve(path)[Node] match {
      case NoNode                     => doesNotExist
      case Dir(kids) if kids.nonEmpty => notEmpty
      case _                          => effect(eok)(remove(path))
    }

  def unlink(path: String): Int =
    resolve(path)[Node] match {
      case NoNode => doesNotExist
      case _      => effect(eok)(remove(path))
    }

  def chmod(path: String, mode: ModeInfo): Int =
    resolve(path)[Node] match {
      case NoNode => doesNotExist
      case _      => effect(eok)(update(path, UnixPerms(mode.mode)))
    }

  def chown(path: String, uid: Long, gid: Long): Int =
    resolve(path)[Node] match {
      case NoNode => doesNotExist
      case _      => effect(eok)(update(path, Uid(uid.toInt), Gid(gid.toInt)))
    }

  def truncate(path: String, size: Long): Int =
    resolve(path) |> { metadata =>
      metadata[Node] match {
        case NoNode  => doesNotExist
        case File(_) => effect(eok)(update(path, Size(size)))
        case _       => isNotValid
      }
    }

  def ftruncate(path: String, size: Long, info: FileInfo): Int = truncate(path, size)

  def utimens(path: String, wrapper: TimeBufferWrapper) =
    resolve(path)[Node] match {
      case NoNode => doesNotExist
      case _      => effect(eok)(update(path, Mtime(FileTime.nanos(wrapper.mod_nsec))))
    }

  private def populateStat(stat: StatInfo, node: Node, metadata: api.Metadata) =
    metadata foreach {
      case Size(bytes)        => stat size   bytes
      case Birth(timestamp)   => // not correctly implemented in stat
      case Atime(timestamp)   => stat atime  (timestamp.inSeconds, timestamp.inNanoSeconds)
      case Ctime(timestamp)   => stat ctime  (timestamp.inSeconds, timestamp.inNanoSeconds)
      case Mtime(timestamp)   => stat mtime  (timestamp.inSeconds, timestamp.inNanoSeconds)
      case BlockCount(amount) => stat blocks amount
      case Uid(value)         => stat uid    value
      case Gid(value)         => stat gid    value
      case UnixPerms(mask)    => stat mode   (node.asFuseBits | mask)
      case Nlink(count)       => stat nlink  count
    }

  private def resolve(path: String)                         = fs apply Resolve(toPath(path))
  private def remove(path: String)                          = fs apply Remove(toPath(path))
  private def write(path: String, data: => Array[Byte])     = fs apply Write(toPath(path), data)
  private def createFile(path: String, perms: UnixPerms)    = fs apply CreateFile(toPath(path), perms)
  private def createDir(path: String, perms: UnixPerms)     = fs apply CreateDir (toPath(path), perms)
  private def createLink(path: String, target: String)      = fs apply CreateLink(toPath(path), target)
  private def update(path: String, attributes: Attribute *) = fs apply Update(toPath(path), Metadata(attributes: _*))
  private def move(from: String, to: String)                = fs apply Move(toPath(from), toPath(to))

  private implicit class NodeOps(val node: Node) {
    def asFuseBits = node match {
      case File(_) => Node.File.getBits
      case Dir(_)  => Node.Dir.getBits
      case Link(_) => Node.Link.getBits
      case NoNode  => 0L
    }
  }

  // comments taken from https://www.cs.hmc.edu/~geoff/classes/hmc.cs135.201109/homework/fuse/fuse_doc.html#function-purposes

  def afterUnmount(mountPoint: java.io.File): Unit = {}
  def beforeMount(mountPoint: java.io.File): Unit = {}

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

  //  Called when the filesystem exits.
  def destroy(): Unit = {}

  //  Return statistics about the filesystem. See statvfs(2) for a description of the structure contents.
  //  Usually, you can ignore the path. Not required, but handy for read/write filesystems since this is how
  //  programs like df determine the free space.
  def statfs(path: String, statfs: StatvfsWrapper): Int = eok

  //  This is the same as the access(2) system call. It returns -ENOENT if the path doesn't
  //  exist, -EACCESS if the requested permission isn't available, or 0 for success. Note that
  //  it can be called on files, directories, or any other object that appears in the filesystem.
  //  This call is not required but is highly recommended.
  def access(path: String, access: Int): Int = notImplemented

  //  Open a file. If you aren't using file handles, this function should just check for existence and
  //  permissions and return either success or an error code. If you use file handles, you should also
  //  allocate any necessary structures and set fi->fh. In addition, fi has some other fields that an
  //  advanced filesystem might find useful; see the structure definition in fuse_common.h for very brief
  //  commentary.
  def open(path: String, info: FileInfo): Int = eok

  //  Open a directory for reading.
  def opendir(path: String, info: FileInfo): Int = eok

  //  This function is similar to bmap(9). If the filesystem is backed by a block device, it
  //  converts blockno from a file-relative block number to a device-relative block.
  def bmap(path: String, info: FileInfo): Int = eok

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

  //  Perform a POSIX file-locking operation.
  //
  //  The lock function is somewhat complex. The cmd will be one of F_GETLK, F_SETLK, or F_SETLKW. The
  //  fields in locks are defined in the fcntl(2) manual page; the l_whence field in that structure will
  //  always be SEEK_SET.
  //
  //  For checking lock ownership, the fi->owner argument must be used.
  //
  //  Contrary to what some other documentation states, the FUSE library does not appear to do anything
  //  special to help you out with locking. If you want locking to work, you will need to implement the
  //  lock function. (Persons who have more knowledge of how FUSE locking works are encouraged to contact
  //  me on this topic, since the existing documentation appears to be inaccurate.)
  def lock(path: String, info: FileInfo, command: FlockCommand, flock: FlockWrapper): Int = eok

  //  This is the only FUSE function that doesn't have a directly corresponding system call, although close(2)
  //  is related. Release is called when FUSE is completely done with a file; at that point, you can free up
  //  any temporarily allocated data structures. The IBM document claims that there is exactly one release per
  //  open, but I don't know if that is true.
  def release(path: String, info: FileInfo): Int = eok

  //  This is like release, except for directories.
  def releasedir(path: String, info: FileInfo): Int = eok

  //  Read an extended attribute. See getxattr(2). This should be implemented only if HAVE_SETXATTR is true.
  def getxattr(path: String, xattr: String, filler: XattrFiller, size: Long, position: Long): Int = notImplemented

  //  List the names of all extended attributes. See listxattr(2). This should be implemented only if
  //  HAVE_SETXATTR is true.
  def listxattr(path: String, filler: net.fusejna.XattrListFiller): Int = notImplemented

  def removexattr(path: String, xattr: String): Int = notImplemented

  //  Set an extended attribute. See setxattr(2). This should be implemented only if HAVE_SETXATTR is true.
  def setxattr(path: String, xattr: String, buf: ByteBuffer, size: Long, flags: Int, position: Int): Int = notImplemented
}

class FuseFs(fs: Filesystem, name: String, logging: Boolean) {
  private val fuse = {
    val fuse = new FuseImplementation(fs, name)
    if (logging) fuse.logging else fuse
  }

  def mountBackground(mountPoint: Path): this.type   = doMount(mountPoint, blocking = false)
  def mountBackground(mountPoint: String): this.type = mountBackground(toPath(mountPoint))
  def mount(mountPoint: Path): this.type             = doMount(mountPoint, blocking = true)
  def mount(mountPoint: String): this.type           = mount(toPath(mountPoint))

  def unmountTry(): Unit = (
    if (!fuse.isMounted)
      return
    else if (isMac)
      exec("umount", "-f", mountPoint) orElse exec("diskutil", "unmount", mountPoint)
    else
      exec("fusermount", "-u", mountPoint)
  )

  private def mountPoint = fuse.getMountPoint.getPath

  private def doMount(mountPoint: Path, blocking: Boolean): this.type = {
    addShutdownHook(if (fuse.isMounted) unmountTry())
    fuse.mount(mountPoint.toFile, blocking)
    this
  }
}
