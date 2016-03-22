package sfs
package jio

import java.nio.file._
import javax.naming.SizeLimitExceededException
import jio._
import java.util.concurrent.TimeUnit.SECONDS

class JavaFilesystem(root: Path) extends api.Filesystem {
  type Path = jio.Path
  type Name = String
  type IO   = Array[Byte]

  def resolve(path: Path): api.Metadata =
    try {
      root append path match {
        case path if path.nofollow.exists =>
          import api.attributes._
          val metadata =
            api.Metadata(
              Atime(path.atime to SECONDS),
              Mtime(path.mtime to SECONDS),
              UnixPerms(toUnixMask(path.perms)),
              Uid(path.uid)
            )

               if (path.isFile) metadata set File(Data(path.readAllBytes)) set Size(path.size) set BlockCount(path.blockCount)
          else if (path.isDir ) metadata set Dir(Data(path.ls.map(p => p.filename -> p).toMap)) set Size(path.size)
          else if (path.isLink) metadata set Link(path.readlink)
          else metadata

        case _ => api.Metadata
      }
    } catch { case t: Throwable =>
      println("You have found a bug, please check the stacktrace to figure out what causes it")
      t.printStackTrace()
      api.Metadata
    }
}
