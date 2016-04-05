package sfs
package jio

import api._, fs._, api.attributes._

class JavaFilesystem(root: jio.Path) extends Filesystem with ConcreteActionsOnly {

  private def resolvePath(path: Path): Path =
    root append path

  def handleConcreteAction[A](action: ConcreteAction[A]): A =
    try {
      action mapPath resolvePath match {

        case Resolve(path) if path.nofollow.exists =>

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

        case Resolve(_) =>
          empty[Metadata]

        case Remove(path)             => asUnit(path.delete())
        case Write(path, data)        => asUnit(path.follow write data)
        case CreateFile(path, perms)  => asUnit(path mkfile perms.mask)
        case CreateDir(path, perms)   => asUnit(path mkdir  perms.mask)
        case CreateLink(path, target) => asUnit(path mklink toPath(target))
        case Move(path, to)           => asUnit(path moveTo resolvePath(to))
        case Update(path, metadata)   =>
          metadata foreach {
            case Size(size)       => path truncate size
            case UnixPerms(mask)  => path setPosixFilePermissions toJavaPermissions(mask)
            case Mtime(timestamp) => path setLastModifiedTime timestamp
            case Gid(id)          => path.gid = id
            case Uid(id)          => path.uid = id
          }
      }
   } catch { case t: Throwable =>
     bug(t)
     action.emptyResult
   }

  private def asUnit[A](a: A): Unit = {}

  // we probably need other defaults
  private implicit val _defaultPerms: Empty[UnixPerms] = Empty(UnixPerms(0))

  private def getKidsFrom(path: Path) =
    Try(path.ls.map(p => p.filename).toSet) | Set.empty

  private def bug(t: Throwable): Unit = {
    println("You have found a bug, please check the stacktrace to figure out what causes it")
    t.printStackTrace()
  }
}
