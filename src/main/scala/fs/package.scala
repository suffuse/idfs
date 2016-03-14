package suffuse

package object fs {
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

  def addUnmountHook(fs: FuseFs): Unit =
    scala.sys addShutdownHook ( if (fs.isMounted) fs.unmountTry() )

  // see also: "allow_recursion", "nolocalcaches", "auto_xattr", "sparse"
  def defaultOptions = Array("-o", "direct_io,default_permissions")

  implicit class FuseFilesystemOps(val fs: FuseFilesystem) extends AnyVal {
    def filter(p: String => Boolean): FuseFs    = new FilteredFs(fs, p)
    def filterNot(p: String => Boolean): FuseFs = new FilteredFs(fs, x => !p(x))
  }

  implicit class MetadataOps(val m: jio.Metadata) extends AnyVal {
    import Node._
    // This is not optimal, but follows existing code
    def nodeType: NodeType =
           if (m.isFile        ) File
      else if (m.isDirectory   ) Dir
      else if (m.isSymbolicLink) Link
      else `At some point in time I would love another approach`

    private def `At some point in time I would love another approach` = sys error "Unknown node type"
  }
}
