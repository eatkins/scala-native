package java.nio.file
package spi

import java.util.{HashSet, LinkedList, List, Map, Set}
import java.util.concurrent.ExecutorService

import java.net.URI

import java.io.{FileInputStream, InputStream, OutputStream}
import java.nio.ByteBuffer
import java.nio.file.attribute.{
  BasicFileAttributes,
  FileAttribute,
  FileAttributeView
}
import java.nio.channels.{
  AsynchronousFileChannel,
  FileChannel,
  SeekableByteChannel
}

import scala.scalanative.nio.fs.UnixFileSystemProvider

abstract class FileSystemProvider protected () {

  // def getFileStore(path: Path): FileStore

  def getScheme(): String

  def newFileSystem(uri: URI, env: Map[String, _]): FileSystem

  def getFileSystem(uri: URI): FileSystem

  def getPath(uri: URI): Path

  def newFileSystem(path: Path, env: Map[String, _]): FileSystem =
    throw new UnsupportedOperationException()

  def newInputStream(path: Path, _options: OpenOption*): InputStream = {
    val options =
      if (_options.isEmpty) Seq[OpenOption](StandardOpenOption.READ)
      else _options
    val channel = Files.newByteChannel(path, options:_*)
    new InputStream {
      private val buffer = ByteBuffer.allocate(1)
      override def read(): Int = {
        buffer.position(0)
        val read = channel.read(buffer)
        if (read <= 0) read
        else buffer.get(0) & 0xFF
      }
      override def close(): Unit =
        channel.close()
    }
  }

  def newOutputStream(path: Path, _options: OpenOption*): OutputStream = {
    val options =
      if (_options.isEmpty)
        Seq[OpenOption](StandardOpenOption.CREATE,
                          StandardOpenOption.TRUNCATE_EXISTING,
                          StandardOpenOption.WRITE)
      else _options :+ StandardOpenOption.WRITE
    val channel = Files.newByteChannel(path, options:_*)
    new OutputStream {
      private val buffer = ByteBuffer.allocate(1)
      override def write(b: Int): Unit = {
        buffer.position(0)
        buffer.put(0, b.toByte)
        channel.write(buffer)
      }
      override def close(): Unit =
        channel.close()
    }
  }

  def newFileChannel(path: Path,
                     options: Set[_ <: OpenOption],
                     attrs: FileAttribute[_]*): FileChannel =
    throw new UnsupportedOperationException

  def newAsynchronousFileChannel(
      path: Path,
      options: Set[_ <: OpenOption],
      executor: ExecutorService,
      attrs: FileAttribute[_]*): AsynchronousFileChannel =
    throw new UnsupportedOperationException

  def newByteChannel(path: Path,
                     options: Set[_ <: OpenOption],
                     attrs: FileAttribute[_]*): SeekableByteChannel =
    FileChannel.open(path, options, attrs:_*)

  def newDirectoryStream(
      dir: Path,
      filter: DirectoryStream.Filter[_ >: Path]): DirectoryStream[Path]

  def createDirectory(dir: Path, attrs: FileAttribute[_]*): Unit

  def createSymbolicLink(link: Path,
                         target: Path,
                         attrs: FileAttribute[_]*): Unit =
    throw new UnsupportedOperationException()

  def createLink(link: Path, existing: Path): Unit =
    throw new UnsupportedOperationException()

  def delete(path: Path): Unit

  def deleteIfExists(path: Path): Boolean =
    try {
      delete(path)
      true
    } catch { case _: NoSuchFileException => false }

  def readSymbolicLink(link: Path): Path =
    throw new UnsupportedOperationException

  def copy(source: Path, target: Path, options: CopyOption*): Unit

  def move(source: Path, target: Path, options: CopyOption*): Unit

  def isSameFile(path: Path, path2: Path): Boolean

  def isHidden(path: Path): Boolean

  def checkAccess(path: Path, modes: Array[AccessMode]): Unit

  def getFileAttributeView[V <: FileAttributeView](
      path: Path,
      tpe: Class[V],
      options: LinkOption*): V

  def readAttributes[A <: BasicFileAttributes](path: Path,
                                               tpe: Class[A],
                                               options: LinkOption*): A

  def readAttributes(path: Path,
                     attributes: String,
                     options: LinkOption*): Map[String, Object]

  def setAttribute(path: Path,
                   attribute: String,
                   value: Object,
                   options: LinkOption*): Unit

}

object FileSystemProvider {
  def installedProviders: List[FileSystemProvider] = {
    val list = new LinkedList[FileSystemProvider]
    list.add(new UnixFileSystemProvider())
    list
  }

}
