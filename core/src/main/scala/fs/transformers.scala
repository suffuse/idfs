package sfs
package fs

import api._, attributes._

object transformers {

  trait Transformer extends (Action ~> Action) { self =>
    def apply[A](action: Action[A]): Action[A] = if (transform isDefinedAt action) transform(action) else action
    def transform[A]: Action[A] =?> Action[A]
    def orElse(other: Transformer): Transformer =
      new Transformer {
        def transform[A] = self.transform[A] orElse other.transform[A]
      }
  }

  def map(f: Map[Metadata, Metadata]) = new Transformer {
    def transform[A] = { case action: Resolve => MapAction(action, f) }
  }

  def filter(p: Predicate[Path]) = new Transformer {
    def transform[A] = { case action: Resolve => FilterPath(action, p) }
  }

  object RemoveWrites extends Transformer {
    import `I don't feel like optimizing`.really
    def transform[A] = { case action: Resolve => action map (_.only[UnixPerms] map (_.noWrites)) }
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

      def transform[A] = {
        case Resolve(path) if path.extension == targetExt =>
          MapAction(Resolve(path replaceExtension sourceExt), DataMap(derive))

        case Resolve(path) if path.extension == sourceExt =>
          InstantResult(empty[Metadata])

        case action: Resolve =>
          MapAction(action, DirMap(DirNamesMap(ExtensionMap(sourceExt, targetExt))))
      }
    }

    def proxyWritesWith(translate: Data => Data) = new Transformer {

      def transform[A] = {

        // this actually doesn't work for data that 'streams' in (which is most of the time)
        // imagine a yaml file that is being fed in line by line
        case Write(path, data) if path.extension == targetExt =>
          Write(path replaceExtension sourceExt, translate(data))

        case Move(path, to) if to.extension == targetExt =>
          import `I don't feel like optimizing`.really
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
