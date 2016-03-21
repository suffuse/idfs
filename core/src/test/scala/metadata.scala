package sfs
package tests

import fuse._, jio._, api._
import attributes._
import org.junit._, Assert._
import scala.concurrent.duration._

final class MetadataTests {
  implicit def emptyMtime: Empty[Mtime] = Empty( ??? )

  @Test
  def monadsAndMetadata(): Unit = {
    val stamp = Mtime(FileTime seconds 123)
    var attrs = Metadata set stamp
    assertEquals(attrs[Mtime], stamp)

    attrs = attrs mapOnly { case Attribute(n: Mtime) => Attribute(n + 1.day) }
    assertEquals(attrs[Mtime], stamp + 1.day)

    attrs = attrs set Size(10000L) set (stamp + 7.days)
    assertEquals(attrs[Mtime], stamp + 7.days)
  }
}
