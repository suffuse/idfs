package sfs

import java.util.concurrent.TimeUnit

package object api extends sfs.api.Api {
  implicit class FileTimeOps(val x: FileTime) extends AnyVal {
    def isOlder(that: FileTime) = (x compareTo that) < 0
    def isNewer(that: FileTime) = (x compareTo that) > 0
    def inSeconds: Long         = x to TimeUnit.SECONDS
    def inMillis: Long          = x.toMillis
    def inNanoSeconds: Long     = x to TimeUnit.NANOSECONDS

    def +(amount: Duration): FileTime = fileTimeInMillis(inMillis + amount.toMillis)
  }
}
