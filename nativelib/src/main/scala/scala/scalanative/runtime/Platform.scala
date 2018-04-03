package scala.scalanative
package runtime

import scala.scalanative.native.{CInt, CString, Ptr, extern, name}

@extern
object Platform {
  @name("scalanative_platform_is_mac")
  def isMac(): Boolean = extern

  @name("scalanative_platform_is_windows")
  def isWindows(): Boolean = extern

  @name("scalanative_windows_get_user_lang")
  def windowsGetUserLang(): CString = extern

  @name("scalanative_windows_get_user_country")
  def windowsGetUserCountry(): CString = extern

  @name("scalanative_little_endian")
  def littleEndian(): Boolean = extern

  @name("scalanative_mac_osx_version")
  def macOSXVersion(major: Ptr[CInt],
                    minor: Ptr[CInt],
                    patch: Ptr[CInt]): Unit = extern

  @name("scalanative_mac_osx_tmp_dir")
  def macOSXTmpDir(tmpDir: Ptr[CString]): Unit = extern
}
