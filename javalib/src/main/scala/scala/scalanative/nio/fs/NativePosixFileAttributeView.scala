package scala.scalanative.nio.fs

import java.util.{HashMap, HashSet, Set}
import java.util.concurrent.TimeUnit
import java.nio.file.{LinkOption, Path}
import java.nio.file.attribute._
import java.io.IOException

import scalanative.native._
import scalanative.posix.{grp, pwd, unistd, time, utime}
import scalanative.posix.sys.stat

final class NativePosixFileAttributeView(path: Path, options: Array[LinkOption])
    extends PosixFileAttributeView
    with FileOwnerAttributeView {
      case class Stat(
        st_dev: stat.dev_t,
        st_rdev: stat.dev_t,
        st_ino: stat.ino_t,
        st_uid: stat.uid_t,
        st_gid: stat.gid_t,
        st_size: unistd.off_t,
        st_atime: time.time_t,
        st_mtime: time.time_t,
        st_ctime: time.time_t,
        st_blocks: stat.blkcnt_t,
        st_blksize: stat.blksize_t,
        st_nlink: stat.nlink_t,
        st_mode: stat.mode_t
      ) {
        def this(p: Ptr[stat.stat]) = this(!p._1, !p._2, !p._3, !p._4, !p._5, !p._6, !p._7, !p._8, !p._9, !p._10, !p._11, !p._12, !p._13)
      }
  override val name: String = "posix"

  override def setTimes(lastModifiedTime: FileTime,
                        lastAccessTime: FileTime,
                        createTime: FileTime): Unit = Zone { implicit z =>
    val sb = getStat()

    val buf = alloc[utime.utimbuf]
    !(buf._1) =
      if (lastAccessTime != null) lastAccessTime.to(TimeUnit.SECONDS)
      else !(sb._7)
    !(buf._2) =
      if (lastModifiedTime != null) lastModifiedTime.to(TimeUnit.SECONDS)
      else !(sb._8)
    // createTime is ignored: No posix-y way to set it.
    if (utime.utime(toCString(path.toString), buf) != 0)
      throw new IOException()
  }

  override def setOwner(owner: UserPrincipal): Unit =
    Zone { implicit z =>
      val passwd = getPasswd(toCString(owner.getName))
      if (unistd.chown(toCString(path.toString), !(passwd._2), -1.toUInt) != 0)
        throw new IOException()
    }

  override def setPermissions(perms: Set[PosixFilePermission]): Unit =
    Zone { implicit z =>
      var mask = 0.toUInt
      NativePosixFileAttributeView.permMap.foreach {
        case (flag, value) => if (perms.contains(value)) mask = mask | flag
      }
      if (stat.chmod(toCString(path.toString), mask) != 0) {
        throw new IOException()
      }
    }

  override def getOwner(): UserPrincipal =
    attributes.owner

  override def setGroup(group: GroupPrincipal): Unit =
    Zone { implicit z =>
      val _group = getGroup(toCString(group.getName))
      val err    = unistd.chown(toCString(path.toString), -1.toUInt, !(_group._2))

      if (err != 0) {
        throw new IOException()
      }
    }

  override def readAttributes(): BasicFileAttributes =
    attributes

  private lazy val attributes =
    new PosixFileAttributes {
      private[this] val s = Zone(implicit z => new Stat(getStat()))
      private def fileStat()(implicit z: Zone) = s
        //getStat()

      private def fileMode()(implicit z: Zone) =
        fileStat().st_mode

      private def filePasswd()(implicit z: Zone) =
        getPasswd(fileStat().st_uid)

      private def fileGroup()(implicit z: Zone) =
        getGroup(fileStat().st_gid)

      override def fileKey =
        Zone { implicit z =>
          (fileStat().st_ino).asInstanceOf[Object]
        }

      override def isDirectory =
        Zone { implicit z =>
          stat.S_ISDIR(fileMode()) == 1
        }

      override def isRegularFile =
        Zone { implicit z =>
          stat.S_ISREG(fileMode()) == 1
        }

      override def isSymbolicLink =
        Zone { implicit z =>
          stat.S_ISLNK(fileMode()) == 1
        }

      override def isOther =
        !isDirectory && !isRegularFile && !isSymbolicLink

      override def lastAccessTime =
        Zone { implicit z =>
          FileTime.from(fileStat().st_atime, TimeUnit.SECONDS)
        }

      override def lastModifiedTime =
        Zone { implicit z =>
          FileTime.from(fileStat().st_mtime, TimeUnit.SECONDS)
        }

      override def creationTime =
        Zone { implicit z =>
          FileTime.from(fileStat().st_ctime, TimeUnit.SECONDS)
        }

      override def group = new GroupPrincipal {
        override val getName =
          Zone { implicit z =>
            fromCString(!(fileGroup()._1))
          }
      }

      override def owner = new UserPrincipal {
        override val getName =
          Zone { implicit z =>
            fromCString(!(filePasswd()._1))
          }
      }

      override def permissions =
        Zone { implicit z =>
          val set = new HashSet[PosixFilePermission]
          NativePosixFileAttributeView.permMap.foreach {
            case (flag, value) =>
              if ((fileMode() & flag).toInt != 0) set.add(value)
          }
          set
        }

      override def size =
        Zone { implicit z =>
          fileStat().st_size
        }
    }

  override def asMap(): HashMap[String, Object] = {
    val values =
      List(
        "lastModifiedTime" -> attributes.lastModifiedTime,
        "lastAccessTime"   -> attributes.lastAccessTime,
        "creationTime"     -> attributes.creationTime,
        "size"             -> Long.box(attributes.size),
        "isRegularFile"    -> Boolean.box(attributes.isRegularFile),
        "isDirectory"      -> Boolean.box(attributes.isDirectory),
        "isSymbolicLink"   -> Boolean.box(attributes.isSymbolicLink),
        "isOther"          -> Boolean.box(attributes.isOther),
        "fileKey"          -> attributes.fileKey,
        "permissions"      -> attributes.permissions,
        "group"            -> attributes.group
      )

    val map = new HashMap[String, Object]()
    values.foreach { case (k, v) => map.put(k, v) }
    map
  }

  override def setAttribute(name: String, value: Object): Unit =
    (name, value) match {
      case ("lastModifiedTime", time: FileTime) =>
        setTimes(time, null, null)
      case ("lastAccessTime", time: FileTime) =>
        setTimes(null, time, null)
      case ("creationTime", time: FileTime) =>
        setTimes(null, null, time)
      case ("permissions", permissions: Set[PosixFilePermission @unchecked]) =>
        setPermissions(permissions)
      case ("group", group: GroupPrincipal) =>
        setGroup(group)
      case _ =>
        super.setAttribute(name, value)
    }

  private def getStat()(implicit z: Zone): Ptr[stat.stat] = {
    val buf = alloc[stat.stat]
    val err =
      if (options.contains(LinkOption.NOFOLLOW_LINKS)) {
        stat.lstat(toCString(path.toString), buf)
      } else {
        stat.stat(toCString(path.toString), buf)
      }

    if (err == 0) buf
    else throw new IOException()
  }

  private def getGroup(name: CString)(implicit z: Zone): Ptr[grp.group] = {
    val buf = alloc[grp.group]
    val err = grp.getgrnam(name, buf)

    if (err == 0) buf
    else throw new IOException()
  }

  private def getGroup(gid: stat.gid_t)(implicit z: Zone): Ptr[grp.group] = {
    val buf = alloc[grp.group]
    val err = grp.getgrgid(gid, buf)

    if (err == 0) buf
    else throw new IOException()
  }

  private def getPasswd(name: CString)(implicit z: Zone): Ptr[pwd.passwd] = {
    val buf = alloc[pwd.passwd]
    val err = pwd.getpwnam(name, buf)

    if (err == 0) buf
    else throw new IOException()
  }

  private def getPasswd(uid: stat.uid_t)(implicit z: Zone): Ptr[pwd.passwd] = {
    val buf = alloc[pwd.passwd]
    val err = pwd.getpwuid(uid, buf)

    if (err == 0) buf
    else throw new IOException()
  }

}
private object NativePosixFileAttributeView {
  val permMap =
    List(
      (stat.S_IRUSR, PosixFilePermission.OWNER_READ),
      (stat.S_IWUSR, PosixFilePermission.OWNER_WRITE),
      (stat.S_IXUSR, PosixFilePermission.OWNER_EXECUTE),
      (stat.S_IRGRP, PosixFilePermission.GROUP_READ),
      (stat.S_IWGRP, PosixFilePermission.GROUP_WRITE),
      (stat.S_IXGRP, PosixFilePermission.GROUP_EXECUTE),
      (stat.S_IROTH, PosixFilePermission.OTHERS_READ),
      (stat.S_IWOTH, PosixFilePermission.OTHERS_WRITE),
      (stat.S_IXOTH, PosixFilePermission.OTHERS_EXECUTE)
    )
}
