package sfs

package object fuse {
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

  implicit class FuseFilesystemOps(val fs: FuseFilesystem) {
    def filter(p: String => Boolean): FuseFs    = new FilteredFs(fs, p)
    def filterNot(p: String => Boolean): FuseFs = new FilteredFs(fs, x => !p(x))
  }
}
