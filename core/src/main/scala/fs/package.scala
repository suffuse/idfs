package sfs

import api._

package object fs {
  def extensionsInDir(from: String, to: String) = DirMap(DirNamesMap(ExtensionMap(from, to)))
  def dataOfFiles(using: Data => Data)          = DataMap(using)
  def filterDir(path: Path, p: Predicate[Path]) = DirMap(DirFilter(path, p))
}
