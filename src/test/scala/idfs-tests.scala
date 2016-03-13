package suffuse
package tests

import jio._
import org.junit._, Assert._

final class IdfsTests {
  val mnt = createTempDirectory("idfs")

  @Test
  def mountHomeDir(): Unit = {
    val fs  = idfs(homeDir, mnt)

    fs.mount()
    val s1 = mnt.ls.map(_.filename).sorted.mkString(" ")
    val s2 = homeDir.ls.map(_.filename).sorted.mkString(" ")
    assertEquals(s1, s2)
    fs.unmountTry() // Fails usually otherwise
  }
}
