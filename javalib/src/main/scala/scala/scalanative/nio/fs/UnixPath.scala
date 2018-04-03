package scala.scalanative.nio.fs

import java.io.File
import java.net.URI
import java.nio.file.{FileSystem, Files, LinkOption, Path, WatchEvent, WatchKey}
import java.util.Iterator

import scala.collection.mutable.UnrolledBuffer

class UnixPath(private val fs: UnixFileSystem, private val rawPath: String)
    extends Path {
  import UnixPath._

  lazy val path: String = removeRedundantSlashes(rawPath)

  override def getFileSystem(): FileSystem =
    fs

  override def isAbsolute(): Boolean =
    rawPath.startsWith("/")

  override def getRoot(): Path =
    if (isAbsolute) new UnixPath(fs, "/")
    else null

  override def getFileName(): Path =
    if (path == "/") null
    else if (path.isEmpty) this
    else new UnixPath(fs, path.split("/").last)

  override def getParent(): Path = {
    val nameCount = getNameCount()
    if (nameCount == 0 || (nameCount == 1 && !isAbsolute)) null
    else if (isAbsolute)
      new UnixPath(fs, "/" + subpath(0, nameCount - 1).toString)
    else subpath(0, nameCount - 1)
  }

  override def getNameCount(): Int =
    if (rawPath.isEmpty) 1
    else path.split("/").filter(_.nonEmpty).length

  override def getName(index: Int): Path = {
    val nameCount = getNameCount
    if (index < 0 || nameCount == 0 || index >= nameCount)
      throw new IllegalArgumentException
    else {
      if (rawPath.isEmpty) this
      else new UnixPath(fs, path.split("/").filter(_.nonEmpty)(index))
    }
  }

  override def subpath(beginIndex: Int, endIndex: Int): Path =
    new UnixPath(fs, (beginIndex until endIndex).map(getName).mkString("/"))

  override def startsWith(other: Path): Boolean =
    if (fs.provider == other.getFileSystem.provider) {
      val otherLength = other.getNameCount()
      val thisLength  = getNameCount()

      if (otherLength > thisLength) false
      else if (isAbsolute ^ other.isAbsolute) false
      else {
        (0 until otherLength).forall(i => getName(i) == other.getName(i))
      }
    } else {
      false
    }

  override def startsWith(other: String): Boolean =
    startsWith(new UnixPath(fs, other))

  override def endsWith(other: Path): Boolean =
    if (fs.provider == other.getFileSystem.provider) {
      val otherLength = other.getNameCount()
      val thisLength  = getNameCount()
      if (otherLength > thisLength) false
      else if (!other.isAbsolute) {
        (0 until otherLength).forall(i =>
          getName(thisLength - 1 - i) == other.getName(otherLength - 1 - i))
      } else if (isAbsolute) {
        this == other
      } else {
        false
      }
    } else {
      false
    }

  override def endsWith(other: String): Boolean =
    endsWith(new UnixPath(fs, other))

  override def normalize(): Path =
    new UnixPath(fs, normalized(path))

  override def resolve(other: Path): Path =
    if (other.isAbsolute || path.isEmpty) other
    else if (other.toString.isEmpty) this
    else new UnixPath(fs, rawPath + (if (!rawPath.endsWith("/")) "/" else "") + other.toString())

  override def resolve(other: String): Path =
    resolve(new UnixPath(fs, other))

  override def resolveSibling(other: Path): Path = {
    val parent = getParent()
    if (parent == null) other
    else parent.resolve(other)
  }

  override def resolveSibling(other: String): Path =
    resolveSibling(new UnixPath(fs, other))

  override def relativize(other: Path): Path = {
    if (isAbsolute ^ other.isAbsolute) {
      throw new IllegalArgumentException("'other' is different type of Path")
    } else if (path.isEmpty) {
      other
    } else if (other.startsWith(this)) {
      other.subpath(getNameCount, other.getNameCount)
    } else if (getParent() == null) {
      new UnixPath(fs, "../" + other.toString())
    } else {
      val next = getParent().relativize(other).toString()
      if (next.isEmpty) new UnixPath(fs, "..")
      else new UnixPath(fs, "../" + next)
    }
  }

  override def toAbsolutePath(): Path = {
    if (File.isAbsolute(path)) this else new UnixPath(fs, toFile().getAbsolutePath())
  }

  override def toRealPath(options: Array[LinkOption]): Path = {
    if (options.contains(LinkOption.NOFOLLOW_LINKS)) toAbsolutePath()
    else {
      new UnixPath(fs, toFile().getCanonicalPath()) match {
        case p if Files.exists(p, Array.empty) => p
        case p => throw new java.io.IOException(s"File $p does not exist")
      }
    }
  }

  override def toFile(): File =
    if (isAbsolute) new File(rawPath)
    else new File(s"${fs.defaultDirectory}/$rawPath")

  override def toUri(): URI =
    new URI(scheme = "file",
            userInfo = null,
            host = null,
            port = -1,
            path = toFile().getAbsolutePath(),
            query = null,
            fragment = null)

  override def iterator(): Iterator[Path] =
    new Iterator[Path] {
      val parts = path.split("/")
      private var i: Int              = 0
      override def remove(): Unit     = throw new UnsupportedOperationException()
      override def hasNext(): Boolean = i < parts.size
      override def next(): Path =
        if (hasNext) {
          val name = new UnixPath(fs, parts(i))
          i += 1
          name
        } else {
          throw new NoSuchElementException()
        }
    }

  override def compareTo(other: Path): Int =
    if (fs.provider == other.getFileSystem.provider) {
      this.toString.compareTo(other.toString)
    } else {
      throw new ClassCastException()
    }

  override def equals(obj: Any): Boolean =
    obj match {
      case other: UnixPath =>
        this.fs == other.fs && this.rawPath == other.rawPath
      case _ => false
    }

  override def hashCode(): Int =
    rawPath.##

  override def toString(): String =
    rawPath

}

private object UnixPath {
  def fastNormalize(path: String): String = {
    var i = 0
    var current = path.charAt(i)
    while (i < path.size - 1) {
      val next = path.charAt(i + 1)
      if (current == '/') {
        next match {
          case '/' => return null
          case '.' =>
            if (i < path.size - 2) {
              path.charAt(i + 2) match {
                case '.' | '/' => return null
                case _ =>
              }
            }
          case _ =>
        }
      }
      current = next
      i += 1
    }
    if (path.charAt(i) == '/') path.substring(0, path.length - 1) else path
  }
  def normalized(path: String): String = {
    val res = fastNormalize(path)
    if (res != null) return res
    val absolute = path.startsWith("/")
    val parts = path.split("/")
    //val offsets = new Array[(Int, Int)](parts.size)
    val newParts = new Array[String](parts.size)

    //parts.filterNot(x => x == "." || x == "")
    var i = 0
    var j = 0
    while (i < parts.size) {
      val s = parts(i)
      if (s != "" && s != ".") {
        newParts(j) = s
        j += 1
      }
      i += 1
    }
    var offsetIndex = 0
    var offset = 0
    /*
     *while (i < newParts.size) {
     *  var j = 0
     *  offset = i
     *  if (newParts(i) == "..") {
     *    var depth = 1
     *    while (i + 1 < newParts.size && newParts(i + 1) == ".." ) {
     *      depth += 1
     *      i += 1
     *    }
     *    j = offsetIndex
     *    if (j > 0) {
     *      if ((i - depth) >= offsets(j - 1)._1)
     *    } else {
     *      offsets(offsetIndex) = (offset, depth)
     *      offsetIndex += 1
     *    }
     *  }
     *  i += 1
     *}
     */
    //println(offsets.toIndexedSeq)
    val builder = new StringBuilder
    if (absolute) builder.append("/")
    i = 0
    while (i < j - 1) {
      builder.append(newParts(i))
      builder.append("/")
      i += 1
    }
    if (j > 0) builder.append(newParts(j))
    builder.toString
  }

  def removeRedundantSlashes(str: String): String =
    if (str.length < 2) str
    else {
      var buffer: StringBuffer = null
      var i = str.indexOf("/")
      if (i == -1) return str
      i += 1
      var j = str.indexOf("/", i)
      if (j == -1) return str
      while (j != -1) {
        if (j == i) {
          if (buffer == null) {
            buffer = new StringBuffer(str.length)
            if (i > 1) buffer.append(str.substring(0, i - 1))
          }
        } else if (buffer != null) {
          buffer.append("/")
          buffer.append(str.substring(i, j))
        }
        i = j + 1
        j = str.indexOf("/", i)
      }
      if (buffer != null) {
        if (i < str.length) {
          buffer.append("/")
          buffer.append(str.substring(i))
        }
        val result = buffer.toString
        if (result.length > 1 && result.endsWith("/")) result.init
        else result
      } else {
        str
      }
    }

}
