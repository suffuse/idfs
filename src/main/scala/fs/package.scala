package suffuse

package object fs {
  type DirectoryFiller   = net.fusejna.DirectoryFiller
  type FileInfo          = net.fusejna.StructFuseFileInfo.FileInfoWrapper
  type FlockCommand      = net.fusejna.FlockCommand
  type FlockWrapper      = net.fusejna.StructFlock.FlockWrapper
  type FuseContext       = net.fusejna.StructFuseContext
  type FuseException     = net.fusejna.FuseException
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

  trait FuseFilesystem extends net.fusejna.FuseFilesystem {
    def resolvePath: String => jio.Path
  }

  def addUnmountHook(fs: FuseFs): Unit =
    scala.sys addShutdownHook ( if (fs.isMounted) fs.unmountTry() )

  // see also: "allow_recursion", "nolocalcaches", "auto_xattr", "sparse"
  def defaultOptions = Array("-o", "direct_io,default_permissions")

  implicit class FuseFilesystemOps(val fs: FuseFilesystem) extends AnyVal {
    def filter   (p: jio.Path => Boolean        ): FuseFs = new FilteredFs(fs, p)
    def filterNot(p: jio.Path => Boolean        ): FuseFs = new FilteredFs(fs, x => !p(x))
    def map      (f: jio.Path => jio.Metadataish): FuseFs = new MappedFs(fs, f)
  }

  def writeData(into: jio.ByteBuffer, data: Array[Byte], amount: Long, offset: Long): Int = {
    val totalBytes = if (offset + amount > data.length) data.length - offset else amount
    effect(totalBytes.toInt)(into.put(data, offset.toInt, totalBytes.toInt))
  }
}
