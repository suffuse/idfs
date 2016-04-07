package sfs
package fs

import api._, attributes._

object transformers {

  trait Transformer extends (Action ~> Action) { self =>

    def transform[A]: Action[A] =?> Action[A]

    def apply[A](action: Action[A]): Action[A] = if (transform isDefinedAt action) transform(action) else action

    def orElse(other: Transformer): Transformer =
      new Transformer {
        def transform[A] = self.transform[A] orElse other.transform[A]
      }
  }

  def map(m: Map[Metadata, Metadata]) = new Transformer {
    def transform[A] = { case action: Resolve => action map m }
  }

  def filter(p: Predicate[Path]) = new Transformer {
    def transform[A] = { case action: Resolve => FilterPath(action, p) }
  }

  object RemoveWrites extends Transformer {
    def transform[A] = { case action: Resolve => action map NoWrites }
  }

  class WithExtensionPair(
    sourceExt : String,
    targetExt : String
  ) {

    def asDerivedFilesUsing(derive: Data => Data) = new Transformer {

      // Note to self: I have had this 'add the derived file' instead of 'replacing the
      //               non derived file'. My naive thought was that I could just use
      //               a filter afterwards. That however proved difficult. Either it masked
      //               the source file from this method, it was not executed, or it's
      //               behavior was undone by this transformer.
      //
      // So, future self, how do you propose we handle this? Composability of file
      // system transformations is very important...
      //
      // I am not sure if we need to do anything. It seems not very problematic to
      // create the few lines below in another transformer with different behavior.

      def transform[A] = {
        case Resolve(path) if path.extension == targetExt =>
          Resolve(path replaceExtension sourceExt) map dataOfFiles(using = derive)

        case Resolve(path) if path.extension == sourceExt =>
          InstantResult(empty[Metadata])

        case action: Resolve =>
          action map extensionsInDir(from = sourceExt, to = targetExt)
      }
    }

    def proxyWritesWith(translate: Data => Data) = new Transformer {

      def transform[A] = {

        // this actually doesn't work for data that 'streams' in (which is most of the time)
        // imagine a yaml file that is being fed in line by line
        // not of concern for today, eventually we need to tap into the `open` method of of
        // fuse and be able to buffer stuff before sending it through to the actual filesystem
        case Write(path, data) if path.extension == targetExt =>
          Write(path replaceExtension sourceExt, translate(data))

        case Move(path, to) if to.extension == targetExt =>
          import syntax._
          for {
            metadata <- Resolve(path)
            data     =  metadata[Node] match {
              case File(data) => data.get
              case _          => empty[Data]
            }
            _        <- Write(path, translate(data))
            _        <- Move(path, to replaceExtension sourceExt)
          } yield ()
      }
    }
  }
}
