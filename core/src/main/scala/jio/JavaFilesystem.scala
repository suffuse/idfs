package sfs
package jio

import java.nio.file._
import javax.naming.SizeLimitExceededException
import api._

trait JavaEffects {

  type M[_]

  protected def success[A]: A => M[A]

  def error[A]: Throwable => M[A]

  def success[A](body: => A): M[A] = Try(body) fold (error, success)
}

class FuseEffects extends JavaEffects {
  type M[A] = fuse.Result[A]

  def success[A]: A => fuse.Result[A] = fuse.Success[A]

  def error[A]: Throwable => fuse.Result[A] = { t =>
    // log(t)
    t match{
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
  }
}

class JavaFilesystem[E <: JavaEffects](root: Path, val effects: E) extends api.Filesystem {
  type M[A] = effects.M[A]
  type Path = jio.Path
  type Name = String
  type Key  = jio.Path
  type IO   = Array[Byte]

  import effects._

  def resolve(path: Path): Key = root append path

  def metadata(key: Key): M[api.Metadata] =
    key match {
      case path if path.nofollow.exists => success(path.metadata)
      case path                         => error(notFound(path))
    }

  def lookup(key: Key): M[Data] =
    key match {
      case path if !path.nofollow.exists  => error(notFound(path))
      case path if  path.isFile           => success(File(path.readAllBytes))
      case path if  path.isDir            => success(Dir(path.ls.map(path => path.filename -> path).toMap))
      case path if  path.isLink           => success(Link(path.readlink))
      case path                           => error(new IOException("unknown data type at " + path))
    }

  private def notFound(path: Path) = new NoSuchFileException(path.to_s)
}
