#ifdef __APPLE__
#include <Foundation/Foundation.h>
#include <stdio.h>
typedef const char * CString;

extern "C" {

void scalanative_mac_osx_version(int *major, int *minor, int *patch) {
    NSOperatingSystemVersion version = [[NSProcessInfo processInfo] operatingSystemVersion];
    *major = (int) version.majorVersion;
    *minor = (int) version.minorVersion;
    *patch = (int) version.patchVersion;
}

void scalanative_mac_osx_tmp_dir(CString *tmp_dir) {
    static char tmp[1025];
    confstr(_CS_DARWIN_USER_TEMP_DIR, tmp, 1024);
    *tmp_dir = tmp;
}

}

#endif
