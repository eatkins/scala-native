#include <dirent.h>
#include <string.h>
#include <stdio.h>

#define NAME_MAX 255

struct scalanative_dirent {
    unsigned long long d_ino;  /** file serial number */
    char d_name[NAME_MAX + 1]; /** name of entry */
    short d_type;
    short d_reclen;
    short d_namlen;
};

int scalanative_dt_unknown() {
  return DT_UNKNOWN;
}
int scalanative_dt_fifo() {
  return DT_FIFO;
}
int scalanative_dt_chr() {
  return DT_CHR;
}
int scalanatve_dt_dir() {
  return DT_DIR;
}
int scalanatve_dt_blk() {
  return DT_BLK;
}
int scalanatve_dt_reg() {
  return DT_REG;
}
int scalanatve_dt_lnk() {
  return DT_LNK;
}
int scalanatve_dt_sock() {
  return DT_SOCK;
}
int scalanatve_dt_wht() {
  return DT_WHT;
}

DIR *scalanative_opendir(const char *name) { return opendir(name); }

void scalanative_dirent_init(struct dirent *dirent,
                             struct scalanative_dirent *my_dirent) {
    my_dirent->d_ino = dirent->d_ino;
    strncpy(my_dirent->d_name, dirent->d_name, NAME_MAX);
    //memset(my_dirent->d_name + dirent->d_namlen, '\0', NAME_MAX + 1 - dirent->d_namlen);
    my_dirent->d_name[NAME_MAX] = '\0';
    my_dirent->d_type = dirent->d_type;
    my_dirent->d_reclen = dirent->d_reclen;
    my_dirent->d_namlen = dirent->d_namlen;
}

int scalanative_readdir(DIR *dirp, struct scalanative_dirent *buf) {
    struct dirent orig_buf;
    struct dirent *result = NULL;
    int res = readdir_r(dirp, &orig_buf, &result);
    if (res == 0) {
        scalanative_dirent_init(&orig_buf, buf);
        return 0;
    } else {
        return res;
    }
}

int scalanative_closedir(DIR *dirp) { return closedir(dirp); }
