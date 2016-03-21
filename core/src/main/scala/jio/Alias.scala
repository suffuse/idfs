package sfs
package jio

import java.nio.{ file => jnf }
import java.nio.{ channels => jnc }
import jnf.{ attribute => jnfa }
import api._

trait Alias {
  type jArray[A]        = Array[A with Object]
  type jClass           = java.lang.Class[_]
  type jField           = java.lang.reflect.Field
  type jFilePermissions = jSet[PosixFilePermission]
  type jIterable[+A]    = java.lang.Iterable[A @uV]
  type jIterator[+A]    = java.util.Iterator[A @uV]
  type jLineIterable    = jIterable[_ <: CharSequence]
  type jList[A]         = java.util.List[A]
  type jMap[K, V]       = java.util.Map[K, V]
  type jMethod          = java.lang.reflect.Method
  type jSet[A]          = java.util.Set[A]
  type jStream[+A]      = java.util.stream.Stream[A @uV]
  type jUri             = java.net.URI
  type jUrl             = java.net.URL

  type CopyOption          = jnf.CopyOption
  type DirStreamFilter[A]  = jnf.DirectoryStream.Filter[A]
  type FileStore           = jnf.FileStore
  type FileSystem          = jnf.FileSystem
  type FileSystemProvider  = jnf.spi.FileSystemProvider
  type FileVisitOption     = jnf.FileVisitOption
  type FileVisitor[A]      = jnf.FileVisitor[A]
  type LinkOption          = jnf.LinkOption
  type OpenOption          = jnf.OpenOption
  type Path                = jnf.Path
  type PathDirStream       = jnf.DirectoryStream[Path]
  type NoSuchFileException = jnf.NoSuchFileException

  type AnyFileAttr         = jnfa.FileAttribute[_]
  type BasicFileAttributes = jnfa.BasicFileAttributes
  type FileAttributeView   = jnfa.FileAttributeView
  type FileAttribute[A]    = jnfa.FileAttribute[A]
  type GroupPrincipal      = jnfa.GroupPrincipal
  type PosixFileAttributes = jnfa.PosixFileAttributes
  type PosixFilePermission = jnfa.PosixFilePermission
  type UserPrincipal       = jnfa.UserPrincipal

  type BufferedInputStream  = java.io.BufferedInputStream
  type BufferedReader       = java.io.BufferedReader
  type BufferedWriter       = java.io.BufferedWriter
  type ByteArrayInputStream = java.io.ByteArrayInputStream
  type ByteBuffer           = java.nio.ByteBuffer
  type Charset              = java.nio.charset.Charset
  type File                 = java.io.File
  type FileChannel          = jnc.FileChannel
  type FileInputStream      = java.io.FileInputStream
  type FileOutputStream     = java.io.FileOutputStream
  type IOException          = java.io.IOException
  type InputStream          = java.io.InputStream
  type OutputStream         = java.io.OutputStream
  type SeekableByteChannel  = jnc.SeekableByteChannel
}
