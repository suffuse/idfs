package sfs
package tests
package md

import fuse._, jio._, api._
import org.junit._, Assert._
import scala.concurrent.duration._

final case class Mtime(timestamp: Long) {
  def +(n: Duration): Mtime = Mtime(timestamp + n.toSeconds)
}
final case class Atime(timestamp: Long)
final case class Size(bytes: Long)
final case class Data(bytes: Array[Byte])

final class MetadataTests {
  implicit val mtime = new Key[Mtime]("modification time")
  implicit val size  = new Key[Size]("size in bytes")

  implicit def emptyMtime: Empty[Mtime] = Empty(Mtime(-1L))
  implicit def emptySize: Empty[Size]   = Empty(Size(-1L))

  @Test
  def monadsAndMetadata(): Unit = {
    val stamp = Mtime(123L)
    var attrs = Metadata set stamp
    assertEquals(attrs[Mtime], stamp)

    attrs = attrs mapOnly { case Attribute(n: Mtime) => Attribute(n + 1.day) }
    assertEquals(attrs[Mtime].timestamp, stamp.timestamp + 1.day.toSeconds)

    attrs = attrs set Size(10000L) set Mtime(789L)
    assertEquals(attrs[Mtime], Mtime(789L))
  }
}
