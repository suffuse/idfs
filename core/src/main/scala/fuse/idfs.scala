package sfs
package fuse

import jio._

object idfs {
  def apply(from: Path): idfs = new idfs(???)
  def main(args: Array[String]): Unit = args.toList match {
    case from :: to :: Nil => idfs(toPath(from)).logging() mountForeground toPath(to)
    case _                 => println("Usage: idfs <from> <to>")
  }
}

class idfs private (fs: api.Filesystem {
  type Metadata = api.Metadata[Fuse]
  type M[A] = Result[A]
  type Path = String
  type Name = String
  type IO   = FuseIO
}) extends FuseFsFull {

  def getName: String = "idfs"


  def read(path: String, buf: ByteBuffer, size: Long, offset: Long, info: FileInfo): Int = {
    for {
      key         <- fs resolve path
      fs.File(io) <- fs lookup key
      data        <- io.read
      totalBytes  =  if (offset + size > data.length) data.length - offset else size
      _           =  buf.put(data, offset.toInt, totalBytes.toInt)
    } yield totalBytes.toInt
  }.toInt

  def write(path: String, buf: ByteBuffer, size: Long, offset: Long, info: FileInfo): Int = {
    for {
      key         <- fs resolve path
      fs.File(io) <- fs lookup key
      data        =  new Array[Byte](size.toInt)
      _           =  buf get data
      _           <- io.write(offset, data)
    } yield size.toInt
  }.toInt

  def lock(path: String, info: FileInfo, command: FlockCommand, flock: FlockWrapper): Int = {
    for {
      key         <- fs resolve path
      fs.File(io) <- fs lookup key
      _           <- io.lock()
    } yield eok
  }.toInt

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
    val m = api.Metadata set (Permissions fromBits mode.mode)

    for {
      metadata <-
        mode.`type`() match {
          case FuseDir                 => Success(m set Dir )
          case FuseFile                => Success(m set File)
          case FuseFifo | FuseSocket   => NotSupported
          case FuseBlockDev | FuseLink => NotSupported
        }
      key     <- fs create metadata
      _       <- fs bind (path -> key)
    } yield eok
  }.toInt

  def mkdir(path: String, mode: ModeInfo): Int = {
    for {
      _        <- fs resolve path whenSuccessful AlreadyExists
      metadata =  api.Metadata set (Permissions fromBits mode.mode) set Dir
      key      <- fs create metadata
      _        <- fs bind (path -> key)
    } yield eok
  }.toInt

  def getattr(path: String, stat: StatInfo): Int = {
    for {
      key      <- fs resolve path
      metadata <- fs metadataFor key
      _        <- populateStat(stat, metadata)
    } yield eok
  }.toInt

  def rename(from: String, to: String): Int = {
    for {
      key <- fs resolve from
      _   <- fs unbind (from -> key)
      _   <- fs bind   (to   -> key)
    } yield eok
  }.toInt

  def rmdir(path: String): Int =
    unlink(path)

  def unlink(path: String): Int = {
    for {
      key <- fs resolve path
      _   <- fs unbind (path -> key)
    } yield eok
  }.toInt

  def chmod(path: String, mode: ModeInfo): Int = {
    for {
      key      <- fs resolve path
      metadata <- fs metadataFor key
      _        <- fs update (key, metadata set (Permissions fromBits mode.mode))
    } yield eok
  }.toInt

  def symlink(target: String, linkName: String): Int = {
    for {
      key      <- fs resolve target
      _        <- fs bind (linkName -> key)
    } yield eok
  }.toInt

  def link(from: String, to: String): Int =
    notSupported()

  def truncate(path: String, size: Long): Int = {
    for {
      key         <- fs resolve path
      fs.File(io) <- fs lookup key
      metadata    <- fs metadataFor key
      fileSize    <- metadata fold[Size] (
                      ifValue = x => Success(x.bytes.toInt),
                      orElse  = io.read map (_.size)
                    )
      _           <-
             if (fileSize > size) io truncate size
        else if (fileSize < size) io write (offset = fileSize, data = nullBytes(amount = size - fileSize))
        else Success(unit)
    } yield eok
  }.toInt

  def utimens(path: String, wrapper: TimeBufferWrapper): Int = {
    for {
      key      <- fs resolve path
      metadata <- fs metadataFor key
      _        <- fs update (key, metadata set Mtime(wrapper.mod_nsec))
    } yield eok
  }.toInt

  private def getUID(): Long = if (isMounted) getFuseContext.uid.longValue else 0
  private def getGID(): Long = if (isMounted) getFuseContext.gid.longValue else 0

  private def nullBytes(amount: Long): Array[Byte] = Array.fill(amount.toInt)(0.toByte)

  private def populateStat(stat: StatInfo, metadata: fs.Metadata): Result[Unit] = {
    for {
      _ <- populateMode(stat, metadata)
    } yield {
      metadata foreach[Size]       (stat size  _.bytes    )
      metadata foreach[Atime]      (stat atime _.timestamp)
      metadata foreach[Mtime]      (stat mtime _.timestamp)
      metadata foreach[BlockCount] (stat blocks _.amount  )
      stat nlink  1
      stat uid    getUID
      stat gid    getGID
    }
  }

  private def populateMode(mode: IModeInfo, metadata: fs.Metadata): Result[Unit] = {
    for {
      nodeType    <- metadata fold[NodeType] (ifValue = Success(_), orElse = DoesNotExist)
      permissions =  metadata[Permissions]
    } yield {
      import permissions._

      mode setMode (nodeType.asFuse,
        ownerRead, ownerWrite, ownerExecute,
        groupRead, groupWrite, groupExecute,
        otherRead, otherWrite, otherExecute
      )
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
