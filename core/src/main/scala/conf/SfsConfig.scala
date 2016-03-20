package sfs
package conf

import fuse._, jio._, api._
import com.typesafe.config._, ConfigFactory._
import com.github.kxbmap.configs._

final case class SfsConfig(
  `fuse-options`: Seq[String],
  list: Map[String, Seq[String]]
)
final case class SfsRoot(args: Seq[String]) {
  def create(): FuseFs = args.toList match {
    case Nil          => ???
    case name :: args =>
      val clazz = Class.forName(name)
      val con = clazz.getConstructor(classOf[Array[String]])

      con.newInstance(args.toArray).asInstanceOf[FuseFs]
  }
}

class SfsId(args: Array[String]) extends RootedFs {
  val root    = path(args(0))
  def getName = getClass.shortName
}
class SfsReverse(args: Array[String]) extends RootedFs {
  val root    = path(args(0))
  def getName = getClass.shortName

  override protected def pathBytes(path: Path): Array[Byte] =
    super.pathBytes(path).reverse
}

object cfs {
  implicit class ConfigOps(val c: Config) {
    def mapKeys(f: String => String): Config = {
      val map = new java.util.HashMap[String, ConfigValue]
      c.entrySet.asScala foreach { entry =>
        map.put(f(entry.getKey), entry.getValue)
      }
      ConfigFactory parseMap map
    }
  }
  def make(s: String) = ConfigFactory parseString s mapKeys ("sfs." + _)

  def main(args: Array[String]): Unit = args match {
    case Array(conf, in, mountPoint) =>
      val config   = ConfigFactory parseFile file(conf)
      val indir    = path(in).toRealPath()
      val fallback = make("indir: \"" + indir + "\"")
      val resolved = (config withFallback fallback).resolve
      val root     = resolved.get[SfsConfig]("sfs")

      root.list foreach {
        case (name, args) =>
          val fs = SfsRoot(args).create()
          val mnt = path(mountPoint) / name
          if (!mnt.isDir) mnt.createDirectories()
          fs mount mnt
      }

  }
}
