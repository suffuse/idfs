package sfs

package object fs {

  // This file exists to experiment with transforming parts of the file system, in this case the Path
  implicit class Wrapped[FS <: api.Filesystem](val underlying: FS) {

    def withMappedPath[T](
      pathFromT: T => underlying.Path,
        pathToT: underlying.Path => T
    ) = new api.Filesystem {

      type Path = T

      type Name = underlying.Name
      type IO   = underlying.IO

      def resolve(path: Path): api.Metadata = {
        val metadata = underlying resolve pathFromT(path)
        val newNode = metadata[underlying.Node] match {
          case underlying.File(data)   => File(Data(data.io))
          case underlying.Dir (kids)   => Dir(Data(kids.io mapValues pathToT))
          case underlying.Link(target) => Link(pathToT(target))
          case underlying.NoNode       => NoNode
        }
        metadata.drop[underlying.Node] set newNode
      }
    }
  }
}
