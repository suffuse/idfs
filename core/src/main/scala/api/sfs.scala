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
        ops transform transformer andThen transformers.RemoveWrites

      def map(f: Map[Metadata, Metadata]) =
        this transform transformers.map(f)

      def filter(p: Predicate[Path]) =
        this transform transformers.filter(p)

      def filterNot(p: Predicate[Path]) =
        filter(Not(p))

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
      case a: Transformation[A] => apply(a.defaultResult)
      case InstantResult(a)     => a
      case FlatMapAction(a, m)  => apply(apply(a) |> m.asFunction)
      case     MapAction(a, m)  =>       apply(a) |> m.asFunction
      case     Execution(a, m)  => m.asFunction apply a
      case        Zipped(a, b)  => (apply(a), apply(b))
    }
}
