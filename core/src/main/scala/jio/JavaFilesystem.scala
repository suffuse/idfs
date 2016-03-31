package sfs
package jio

import api._, api.attributes._

class JavaFilesystem(root: jio.Path) extends Filesystem {

  private def resolvePath(path: Path): Path =
    root append path

  def resolve(path: Path): Metadata =
    try {
      resolvePath(path) match {
        case path if path.nofollow.exists =>

          val metadata = Metadata(
            Atime(path.atime),
            Mtime(path.mtime),
            UnixPerms(toUnixMask(path.perms)),
            Uid(path.uid),
            Nlink(path.nlink)
          )

               if (path.isFile) metadata set File(path.readAllBytes) set Size(path.size) set BlockCount(path.blockCount)
          else if (path.isDir ) metadata set Dir (getKidsFrom(path)) set Size(path.size)
          else if (path.isLink) {
            metadata set Link(path.readlink.to_s)
          }
          else metadata

        case _ =>
          Metadata
      }
    } catch { case t: Throwable =>
      bug(t)
      Metadata
    }

  private def getKidsFrom(path: Path) =
    Try(path.ls.map(p => p.filename).toSet) | Set.empty

  private def bug(t: Throwable): Unit = {
    println("You have found a bug, please check the stacktrace to figure out what causes it")
    t.printStackTrace()
  }
}

class JavaStore(root: jio.Path) extends Store {

  private def resolvePath(path: Path): Path =
    root append path

  def update(path: Path, metadata: Metadata): Unit =
    try {
      resolvePath(path) match {
        case path if path.nofollow.exists =>
          updateNode(path, metadata)

        case path =>
          newNode(path, metadata)
      }
    } catch { case t: Throwable =>
      bug(t)
    }

  def move(oldPath: Path, newPath: Path): Unit =
    resolvePath(oldPath) moveTo resolvePath(newPath)

  private def newNode(path: Path, metadata: Metadata) =
    metadata[Node] match {
      case NoNode =>
        // easy, don't do anything

      case File(data) =>
        (path mkfile metadata[UnixPerms].mask).nofollow write data.get

      case Dir(kids) if kids.nonEmpty => throw new UnsupportedOperationException("we do not support creating a dir with kids")
      case Dir(_) =>
        path mkdir metadata[UnixPerms].mask

      case Link(target) =>
        path mklink jio.path(target)
    }

  private def updateNode(path: Path, metadata: Metadata) =
    metadata.foreach {
      case NoNode           => path.delete()
      case Size(size)       => path truncate size
      case UnixPerms(mask)  => path setPosixFilePermissions toJavaPermissions(mask)
      case Mtime(timestamp) => path setLastModifiedTime timestamp
      case File(data)       => path.follow write data.get
    }

  // we probably need other defaults
  private implicit val _defaultPerms: Empty[UnixPerms] = Empty(UnixPerms(0))

  private def bug(t: Throwable): Unit = {
    println("You have found a bug, please check the stacktrace to figure out what causes it")
    t.printStackTrace()
  }
}
