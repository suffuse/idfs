package sfs

package object fs {

  // This file exists to experiment with transforming parts of the file system, in this case the Path
  implicit class Wrapped[FS <: api.Filesystem](val underlying: FS) {

    def withMappedPath[T](
      pathFromT: T => underlying.Path,
        pathToT: underlying.Path => T
    )(implicit F: api.Functor[underlying.M]) = new api.Filesystem {

      type Path = T

      type M[A] = underlying.M[A]
      type Name = underlying.Name
      type Key  = underlying.Key
      type IO   = underlying.IO

      def resolve(path: Path): Key            = underlying resolve pathFromT(path)
      def metadata(key: Key): M[api.Metadata] = underlying metadata key
      def lookup(key: Key): M[Data]           = underlying lookup key map {
        case underlying.Link(path)    => Link(pathToT(path))
        case underlying.File(io)      => File(io)
        case underlying.Dir(children) => Dir(children)
      }
    }
  }
}
