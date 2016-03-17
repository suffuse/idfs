package suffuse
package api

trait ShowDirect extends Any { def to_s: String }
trait ShowSelf extends Any with ShowDirect { override def toString = to_s }
