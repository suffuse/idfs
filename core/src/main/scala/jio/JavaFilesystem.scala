package sfs
package jio

import java.nio.file._
import java.nio.{ file => jnf }
import jnf.{ attribute => jnfa }
import javax.naming.SizeLimitExceededException
import api._

trait JavaBinding[Z] {
  type M[_]
  type IO
  protected def success[A]: A         => M[A]
  def error  [A]: Throwable => M[A]

  def toMetadata(jm: Metadata[Java]): Metadata[Z]
  def fromMetadata(m: Metadata[Z]): Metadata[Java]
  def update(path: jio.Path, jm: Metadata[Java]): M[Unit]
  def toIO(path: jio.Path): IO

  def safe[A](body: => A): M[A] = Try(body) fold (error, success)
}

class FuseJavaBinding extends JavaBinding[fuse.Fuse] {
  type M[A] = fuse.Result[A]
  type IO = fuse.FuseIO

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

  def toMetadata(jm: Metadata[Java]): Metadata[fuse.Fuse] =
    ???

  def fromMetadata(m: Metadata[fuse.Fuse]): Metadata[Java] =
    ???

  def update(path: jio.Path, jm: Metadata[Java]): M[Unit] = ???

  def toIO(path: jio.Path): IO = ???

  private def toPermissions(pfp: Set[jnfa.PosixFilePermission]) = {
    import jnfa.PosixFilePermission._
    fuse.Permissions(
      pfp(GROUP_READ) , pfp(GROUP_WRITE) , pfp(GROUP_EXECUTE),
      pfp(OWNER_READ) , pfp(OWNER_WRITE) , pfp(OWNER_EXECUTE),
      pfp(OTHERS_READ), pfp(OTHERS_WRITE), pfp(OTHERS_EXECUTE)
    )
  }
}

// So this is fucked up because a with with metadata does not point to the same thing
// after it has been stored. We should probably store a temp file somewhere with the
// metadata and move it on first bind.
case class JavaKey(value: jio.Path | Metadata[Java])

class JavaFuseFilesystem[Z](val binding: JavaBinding[Z]) extends Filesystem {

  type Context = Z
  type Metadata = api.Metadata[Z]

  import binding._

  type Key = JavaKey
  val  Key = JavaKey

  type M[A] = binding.M[A]
  type IO   = binding.IO
  // for now I'm using these hardcoded. They could be moved to the binding
  type Path = String
  type Name = String

  def resolve(path: Path): M[Key] = {
    val p = toPath(path)
    if (p.exists) safe(Key(p)) else error(notFound(path))
  }
  def lookup(key: Key): M[Data] =
    key.value.fold(
      {
        case path if path.isFile => safe(File(toIO(path)))
        case path                => error(new NoSuchFileException(path.to_s))
      },
      metadata => error(new jio.IOException)
    )

  def metadataFor(key: Key): M[Metadata] =
    key.value.fold(
      {
        case path if path.exists => safe(toMetadata(path.metadata))
        case path                => error(notFound(path.to_s))
      },
      metadata => safe(toMetadata(metadata))
    )

  def create(metadata: Metadata): M[Key] =
    safe(Key(fromMetadata(metadata)))

  def update(key: Key, metadata: Metadata): M[Unit] =
    key.value.fold(
      {
        case path if path.exists => binding.update(path, fromMetadata(metadata))
        case path                => error(notFound(path.to_s))
      },
      metadata => error(new jio.IOException("Trying to update a key that has not been stored"))
    )

  def bind  (binding: Path -> Key): M[Unit] = {
    val (path, key) = binding
    key.value.fold(
      keyPath => safe(toPath(path) mklink keyPath),
      {
        case metadata if metadata has jio.File => safe(toPath(path) mkfile metadata)
        case metadata if metadata has jio.Dir  => safe(toPath(path) mkdir metadata)
        case metadata => error(new jio.IOException(s"Binding failed: $path to $metadata"))
      }
    )
  }

  def unbind(binding: Path -> Key): M[Unit] = {
    val (path, key) = binding
    key.value.fold(
      {
        case keyPath if keyPath == toPath(path) => safe(keyPath.delete())
        case _ if toPath(path).isSymbolicLink   => safe(toPath(path).delete())
        case keyPath => error(new jio.IOException(s"Trying to unbind unrelated paths $keyPath and $path"))
      },
      {
        case metadata => safe(toPath(path).delete())
      }
    )
  }

  private def notFound(path: String) = new NoSuchFileException(path)
}
