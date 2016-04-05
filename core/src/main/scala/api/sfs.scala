package sfs
package api

import attributes._, fs._

trait Filesystem {
  def apply[A](action: Action[A]): A
}

object Filesystem {

  implicit class FilesystemOps(fs: Filesystem) {

    class TransformedFilesystem(transformer: Action ~> Action) extends Filesystem {
      def apply[A](action: Action[A]): A = fs apply transformer(action)
      def andThen(next: Action ~> Action) = new TransformedFilesystem(transformer andThen next)
    }

    def transform(transformer: Action ~> Action) = new TransformedFilesystem(transformer)

    def map(f: Map[Metadata, Metadata]) =
      this transform transformers.map(f)

    def filter(p: Predicate[Path]) =
      this transform transformers.filter(p)

    def filterNot(p: Predicate[Path]) = filter(Not(p))
  }
}

trait ConcreteActionsOnly { _: Filesystem =>

  protected def handleConcreteAction[A](action: ConcreteAction[A]): A

  def apply[A](action: Action[A]): A =
    action match {
      case a: ConcreteAction[A] => handleConcreteAction(a)
      case InstantResult(a)     => a
      case FlatMapAction(a, f)  => apply(apply(a) |> f)
      case MapAction(a, f)      => apply(a) |> f
      case a: Transformation[A] => apply(a.defaultResult)
    }
}
