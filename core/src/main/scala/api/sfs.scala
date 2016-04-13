package sfs
package api

import attributes._, fs._

trait Filesystem {
  def apply[A](action: Action[A]): A
}

object Filesystem {

  implicit class FilesystemOps(fs: Filesystem) { ops =>

    def transform(transformer: Action ~> Action) =
      new TransformedFilesystem(fs, transformer)

    object reads extends TransformedFilesystem(fs, transformers.RemoveWrites) {
      def transform(transformer: Action ~> Action) =
        this andThen transformer

      def map(f: Action[Metadata] => Action[Metadata]) =
        this transform transformers.map(f)

      def filter(p: Path => Boolean) =
        this transform transformers.filter(p)

      def filterNot(p: Path => Boolean) =
        filter(p andThen (!_))

      def concat(other: Filesystem) =
        this transform transformers.concat(other)
    }
  }
}

class TransformedFilesystem(fs: Filesystem, transformer: Action ~> Action) extends Filesystem {
  def apply[A](action: Action[A]): A  = fs apply transformer(action)
  def andThen(next: Action ~> Action) = new TransformedFilesystem(fs, transformer andThen next)
}

trait ConcreteActionsOnly { _: Filesystem =>

  protected def handleConcreteAction[A](action: ConcreteAction[A]): A

  def apply[A](action: Action[A]): A =
    action match {
      case a: ConcreteAction[A] => handleConcreteAction(a)
      case InstantResult(a)     => a
      case FlatMapAction(a, f)  => apply(apply(a) |> f)
    }
}
