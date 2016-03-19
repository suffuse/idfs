package sfs
package jio

import java.nio.file._
import javax.naming.SizeLimitExceededException
import api._

trait JavaBinding {
  type M[_]
  def success[A]: A         => M[A]
  def error  [A]: Throwable => M[A]

  def tryFuse[A](body: => A): M[A] = Try(body) fold (error, success)
}

class FuseJavaBinding extends JavaBinding {
  type M[A] = fuse.Result[A]

  def success[A]: A => M[A] = fuse.Success[A]

  def error[A]: Throwable => M[A] = {
    case _: FileAlreadyExistsException    => fuse.AlreadyExists
    case _: NoSuchFileException           => fuse.DoesNotExist
    case _: IllegalArgumentException      => fuse.NotValid
    case _: UnsupportedOperationException => fuse.NotImplemented
    case _: DirectoryNotEmptyException    => fuse.NotEmpty
    case _: SizeLimitExceededException    => fuse.TooBig
    case _: AccessDeniedException         => fuse.AccessDenied
    case _: jio.IOException               => fuse.InputOutputError
    case _                                => fuse.InputOutputError
  }

  def metadataFor(key: JavaKey): M[Metadata] =
    key.value.fold(
      path => ???, // construct using path
      metadata => success(metadata)
    )
}

case class JavaKey(value: jio.Path | Metadata)

class JavaFuseFilesystem(val binding: JavaBinding) extends Filesystem {

  import binding._

  type Key = JavaKey
  val  Key = JavaKey

  type M[A] = binding.M[A]
  // for now I'm using these hardcoded. They could be moved to the binding
  type Path = String
  type Name = String
  type IO   = fuse.FuseIO

  def resolve(path: Path): M[Key] = {
    val p = toPath(path)
    if (p.exists) success(Key(p)) else error(new NoSuchFileException(path))
  }
  def lookup(key: Key): M[Data] =
    key.value.fold(
      path => ???,    // get data from path
      metadata => ??? // if it's a file, return an empty byte array, else ...
    )
  def metadataFor(key: Key): M[Metadata] = metadataFor(key)

  def create(metadata: Metadata): M[Key] = success(Key(metadata))

  def update(key: Key, metadata: Metadata): M[Unit] =
    ??? // here we expect the key to have a path attached

  def bind  (binding: Path -> Key): M[Unit] =
    ??? // if key contains metadata we can store it in the file system
        // otherwise, it it's a path and it's different, we return an error.
        // In the future we might support hard links
  def unbind(binding: Path -> Key): M[Unit] =
    ??? // without support for hardlinks, this means that whatever is at the
        // path can be removed
}
