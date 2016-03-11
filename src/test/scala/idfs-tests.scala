package suffuse
package tests

import jio._
import scala.sys.process.Process

final class IdfsTests {
  val mnt = createTempDirectory("idfs")

  @Test
  def mountHomeDir(): Unit = {
    val fs  = new idfs(homeDir, mnt)
    fs.mount()
    assert(Process("diff", "-r", homeDir, mnt).!! == 0)
    fs.unmount()
  }
}
