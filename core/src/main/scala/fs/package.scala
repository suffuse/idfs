package sfs

import api._

package object fs {

  // This file exists to experiment with transforming parts of the file system, in this case the Path
  implicit class Wrapped[FS <: Filesystem](val fs: FS) {

    import fs._

    def map(f: Metadata => Metadata) =
      new Filesystem {
        type Path = fs.Path
        type Data = fs.Data

        def resolve(path: Path) =
          f(fs resolve path).only[fs.Node] map {
            case fs.File(data)   => File(data.get)
            case fs.Dir (kids)   => Dir (kids.get)
            case fs.Link(target) => Link(target)
            case fs.NoNode       => NoNode
          }
      }

    def contraMap[T](toNewPath: fs.Path => T, fromNewPath: T => fs.Path) =
      new Filesystem {
        type Path = T
        type Data   = fs.Data

        def resolve(path: Path) =
          (fs resolve fromNewPath(path)).only[fs.Node] map {
            case fs.File(data)   => File(data.get)
            case fs.Dir (kids)   => Dir (kids.get mapValues toNewPath)
            case fs.Link(target) => Link(toNewPath(target))
            case fs.NoNode       => NoNode
          }
      }

    def withMappedPath[T](toNewPath: Path => T, fromNewPath: T => Path) =
      contraMap(toNewPath, fromNewPath)

    def mapNode(f: fs.Node =?> fs.Node) = map(_.only[fs.Node] mapOnly f)

    def filter(p: fs.Path => Boolean) = mapNode {
      case dir: fs.Dir => dir filter p
    }
    def filterNot(p: fs.Path => Boolean) = filter(x => !p(x))

  }
}
