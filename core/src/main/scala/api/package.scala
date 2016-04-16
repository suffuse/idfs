package sfs

import java.nio.file.{ attribute => jnfa }
import java.util.concurrent.TimeUnit

package object api extends sfs.api.Api {

  implicit class PathOps(path: Path) extends jio.PathOpsInstance(path)

  implicit class FileTimeOps(val x: FileTime) extends AnyVal {
    def isOlder(that: FileTime) = (x compareTo that) < 0
    def isNewer(that: FileTime) = (x compareTo that) > 0
    def inSeconds: Long         = x to TimeUnit.SECONDS
    def inMillis: Long          = x.toMillis
    def inNanoSeconds: Long     = x to TimeUnit.NANOSECONDS

    def +(amount: Duration): FileTime = FileTime.millis(inMillis + amount.toMillis)
  }
}

package api {
  object FileTime {
    def millis(ms: Long): FileTime    = jnfa.FileTime fromMillis ms
    def nanos(nanos: Long): FileTime  = jnfa.FileTime.from(nanos, TimeUnit.NANOSECONDS)
    def seconds(secs: Long): FileTime = jnfa.FileTime.from(secs, TimeUnit.SECONDS)
  }
}
