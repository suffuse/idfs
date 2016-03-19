package sfs
package jio

import java.nio.{ file => jnf }
import jnf.{ Files => F }
import jnf.LinkOption.NOFOLLOW_LINKS

// LinkOption <: ( CopyOption, OpenOption )
//
// Link options: NOFOLLOW_LINKS
// Copy options: ATOMIC_MOVE, COPY_ATTRIBUTES, REPLACE_EXISTING
// Open options: APPEND CREATE CREATE_NEW DELETE_ON_CLOSE DSYNC READ SPARSE SYNC TRUNCATE_EXISTING WRITE
class JioFollow(path: Path) {
  def exists(): Boolean                                                 = F.exists(path)
  def getAttribute(attribute: String): Object                           = F.getAttribute(path, attribute)
  def getFileAttributeView[V <: FileAttributeView](`type`: Class[V]): V = F.getFileAttributeView(path, `type`)
  def getLastModifiedTime(): FileTime                                   = F.getLastModifiedTime(path)
  def getOwner(): UserPrincipal                                         = F.getOwner(path)
  def getPosixFilePermissions(): jFilePermissions                       = F.getPosixFilePermissions(path)
  def isDirectory(): Boolean                                            = F.isDirectory(path)
  def isRegularFile(): Boolean                                          = F.isRegularFile(path)
  def notExists(): Boolean                                              = F.notExists(path)
  def readAttributes(attributes: String): jMap[String, Object]          = F.readAttributes(path, attributes)
  def readAttributes[A <: BasicFileAttributes](`type`: Class[A]): A     = F.readAttributes(path, `type`)
  def setAttribute[A](attribute: String, value: A): Path                = F.setAttribute(path, attribute, value)

  private def mustFollow[A](opts: Seq[A]): Seq[A]      = opts filterNot (_ == NOFOLLOW_LINKS)
  private def mustFollowSet[A](opts: jSet[A]): jSet[A] = opts.asScala filterNot (_ == NOFOLLOW_LINKS) asJava

  def copy(target: Path, options: CopyOption*): Path                                           = F.copy(path, target, mustFollow(options): _*)
  def move(target: Path, options: CopyOption*): Path                                           = F.move(path, target, mustFollow(options): _*)
  def newBufferedWriter(cs: Charset, options: OpenOption*): BufferedWriter                     = F.newBufferedWriter(path, cs, mustFollow(options): _*)
  def newByteChannel(options: OpenOption*): SeekableByteChannel                                = F.newByteChannel(path, mustFollow(options): _*)
  def newByteChannel(options: jSet[_ <: OpenOption], attrs: AnyFileAttr*): SeekableByteChannel = F.newByteChannel(path, mustFollowSet(options), attrs: _*)
  def newInputStream(options: OpenOption*): InputStream                                        = F.newInputStream(path, mustFollow(options): _*)
  def newOutputStream(options: OpenOption*): OutputStream                                      = F.newOutputStream(path, mustFollow(options): _*)
  def write(bytes: Array[Byte], options: OpenOption*): Path                                    = F.write(path, bytes, mustFollow(options): _*)
  def write(lines: jLineIterable, cs: Charset, options: OpenOption*): Path                     = F.write(path, lines, cs, mustFollow(options): _*)
}

class JioNoFollow(path: Path) {
  def exists(): Boolean                                                 = F.exists(path, NOFOLLOW_LINKS)
  def getAttribute(attribute: String): Object                           = F.getAttribute(path, attribute, NOFOLLOW_LINKS)
  def getFileAttributeView[V <: FileAttributeView](`type`: Class[V]): V = F.getFileAttributeView(path, `type`, NOFOLLOW_LINKS)
  def getLastModifiedTime(): FileTime                                   = F.getLastModifiedTime(path, NOFOLLOW_LINKS)
  def getOwner(): UserPrincipal                                         = F.getOwner(path, NOFOLLOW_LINKS)
  def getPosixFilePermissions(): jFilePermissions                       = F.getPosixFilePermissions(path, NOFOLLOW_LINKS)
  def isDirectory(): Boolean                                            = F.isDirectory(path, NOFOLLOW_LINKS)
  def isRegularFile(): Boolean                                          = F.isRegularFile(path, NOFOLLOW_LINKS)
  def notExists(): Boolean                                              = F.notExists(path, NOFOLLOW_LINKS)
  def readAttributes(attributes: String): jMap[String, Object]          = F.readAttributes(path, attributes, NOFOLLOW_LINKS)
  def readAttributes[A <: BasicFileAttributes](`type`: Class[A]): A     = F.readAttributes(path, `type`, NOFOLLOW_LINKS)
  def setAttribute[A](attribute: String, value: A): Path                = F.setAttribute(path, attribute, value, NOFOLLOW_LINKS)

  private def mustNotFollowSet[A <: OpenOption](opts: jSet[A]): jSet[_ <: OpenOption] =
    if (opts contains NOFOLLOW_LINKS) opts else jSet(opts.asScala.toArray[OpenOption] :+ NOFOLLOW_LINKS: _*)

  def copy(target: Path, options: CopyOption*): Path                                           = F.copy(path, target, NOFOLLOW_LINKS +: options: _*)
  def move(target: Path, options: CopyOption*): Path                                           = F.move(path, target, NOFOLLOW_LINKS +: options: _*)
  def newBufferedWriter(cs: Charset, options: OpenOption*): BufferedWriter                     = F.newBufferedWriter(path, cs, NOFOLLOW_LINKS +: options: _*)
  def newByteChannel(options: OpenOption*): SeekableByteChannel                                = F.newByteChannel(path, NOFOLLOW_LINKS +: options: _*)
  def newByteChannel(options: jSet[_ <: OpenOption], attrs: AnyFileAttr*): SeekableByteChannel = F.newByteChannel(path, mustNotFollowSet(options), attrs: _*)
  def newInputStream(options: OpenOption*): InputStream                                        = F.newInputStream(path, NOFOLLOW_LINKS +: options: _*)
  def newOutputStream(options: OpenOption*): OutputStream                                      = F.newOutputStream(path, NOFOLLOW_LINKS +: options: _*)
  def write(bytes: Array[Byte], options: OpenOption*): Path                                    = F.write(path, bytes, NOFOLLOW_LINKS +: options: _*)
  def write(lines: jLineIterable, cs: Charset, options: OpenOption*): Path                     = F.write(path, lines, cs, NOFOLLOW_LINKS +: options: _*)
}

class JioFilesInstance(path: Path) {
  def follow: JioFollow     = new JioFollow(path)
  def nofollow: JioNoFollow = new JioNoFollow(path)

  def createDirectories(attrs: AnyFileAttr*): Path                              = F.createDirectories(path, attrs: _*)
  def createDirectory(attrs: AnyFileAttr*): Path                                = F.createDirectory(path, attrs: _*)
  def createFile(attrs: AnyFileAttr*): Path                                     = F.createFile(path, attrs: _*)
  def createSymbolicLink(target: Path, attrs: AnyFileAttr*): Path               = F.createSymbolicLink(path, target, attrs: _*)
  def createTempDirectory(prefix: String, attrs: AnyFileAttr*): Path            = F.createTempDirectory(path, prefix, attrs: _*)
  def createTempFile(prefix: String, suffix: String, attrs: AnyFileAttr*): Path = F.createTempFile(path, prefix, suffix, attrs: _*)

  def copy(out: OutputStream): Long                                                                      = F.copy(path, out)
  def createLink(existing: Path): Path                                                                   = F.createLink(path, existing)
  def delete(): Unit                                                                                     = F.delete(path)
  def deleteIfExists(): Boolean                                                                          = F.deleteIfExists(path)
  def getFileStore(): FileStore                                                                          = F.getFileStore(path)
  def isExecutable(): Boolean                                                                            = F.isExecutable(path)
  def isHidden(): Boolean                                                                                = F.isHidden(path)
  def isReadable(): Boolean                                                                              = F.isReadable(path)
  def isSameFile(path2: Path): Boolean                                                                   = F.isSameFile(path, path2)
  def isSymbolicLink(): Boolean                                                                          = F.isSymbolicLink(path)
  def isWritable(): Boolean                                                                              = F.isWritable(path)
  def newBufferedReader(cs: Charset): BufferedReader                                                     = F.newBufferedReader(path, cs)
  def newDirectoryStream(): PathDirStream                                                                = F.newDirectoryStream(path)
  def newDirectoryStream(filter: DirStreamFilter[_ >: Path]): PathDirStream                              = F.newDirectoryStream(path, filter)
  def newDirectoryStream(glob: String): PathDirStream                                                    = F.newDirectoryStream(path, glob)
  def probeContentType(): String                                                                         = F.probeContentType(path)
  def readAllBytes(): Array[Byte]                                                                        = F.readAllBytes(path)
  def readAllLines(cs: Charset): jList[String]                                                           = F.readAllLines(path, cs)
  def readSymbolicLink(): Path                                                                           = F.readSymbolicLink(path)
  def setLastModifiedTime(time: FileTime): Path                                                          = F.setLastModifiedTime(path, time)
  def setOwner(owner: UserPrincipal): Path                                                               = F.setOwner(path, owner)
  def setPosixFilePermissions(perms: jFilePermissions): Path                                             = F.setPosixFilePermissions(path, perms)
  def size(): Long                                                                                       = F.size(path)
  def walkFileTree(options: jSet[FileVisitOption], maxDepth: Int, visitor: FileVisitor[_ >: Path]): Path = F.walkFileTree(path, options, maxDepth, visitor)
  def walkFileTree(visitor: FileVisitor[_ >: Path]): Path                                                = F.walkFileTree(path, visitor)
}
