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
            Birth(path.birth),
            Atime(path.atime),
            Ctime(path.ctime),
            Mtime(path.mtime),
            UnixPerms(toUnixMask(path.perms)),
            Uid(path.uid),
            Gid(path.gid),
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

  def update(path: Path, update: Update): Unit =
    try {
      val unresolvedPath = path
      resolvePath(unresolvedPath) |> { path =>
        update match {
          case Remove             => path.delete()
          case Write(data)        => path.follow write data
          case CreateFile(perms)  => path mkfile perms.mask
          case CreateDir(perms)   => path mkdir  perms.mask
          case CreateLink(target) => path mklink toPath(target)
          case Move(to)           => path moveTo resolvePath(to)
          case UpdateAttribute(a) =>
            a.value match {
              case Size(size)       => path truncate size
              case UnixPerms(mask)  => path setPosixFilePermissions toJavaPermissions(mask)
              case Mtime(timestamp) => path setLastModifiedTime timestamp
              case Gid(id)          => path.gid = id
              case Uid(id)          => path.uid = id
            }
          case Multiple(updates)  => updates foreach (this update (unresolvedPath, _))
        }
      }
    } catch { case t: Throwable =>
      bug(t)
    }

  // we probably need other defaults
  private implicit val _defaultPerms: Empty[UnixPerms] = Empty(UnixPerms(0))

  private def getKidsFrom(path: Path) =
    Try(path.ls.map(p => p.filename).toSet) | Set.empty

  private def bug(t: Throwable): Unit = {
    println("You have found a bug, please check the stacktrace to figure out what causes it")
    t.printStackTrace()
  }
}
