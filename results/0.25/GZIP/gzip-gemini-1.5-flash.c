#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <zlib.h>


//Structure to represent a gzip header
typedef struct {
    unsigned char id1;
    unsigned char id2;
    unsigned char cm;
    unsigned char flg;
    unsigned long mtime;
    unsigned char xfl;
    unsigned char os;
    unsigned long extra_len;
    unsigned char *extra;
    unsigned long name_len;
    unsigned char *name;
    unsigned long comment_len;
    unsigned char *comment;
    unsigned int crc;
} gzip_header;


//Function to read gzip header
gzip_header read_gzip_header(int fd) {
    gzip_header header;
    if (read(fd, &header.id1, 1) != 1 || header.id1 != 0x1f ||
        read(fd, &header.id2, 1) != 1 || header.id2 != 0x8b) {
        perror("Error reading gzip header");
        exit(1);
    }
    if (read(fd, &header.cm, 1) != 1 ||
        read(fd, &header.flg, 1) != 1 ||
        read(fd, &header.mtime, 4) != 4 ||
        read(fd, &header.xfl, 1) != 1 ||
        read(fd, &header.os, 1) != 1) {
        perror("Error reading gzip header");
        exit(1);
    }
    header.extra_len = 0;
    header.extra = NULL;
    header.name_len = 0;
    header.name = NULL;
    header.comment_len = 0;
    header.comment = NULL;
    if (header.flg & 4) {
        if (read(fd, &header.extra_len, 2) != 2) {
            perror("Error reading gzip header");
            exit(1);
        }
        header.extra = (unsigned char *)malloc(header.extra_len);
        if (read(fd, header.extra, header.extra_len) != header.extra_len) {
            perror("Error reading gzip header");
            exit(1);
        }
    }
    if (header.flg & 8) {
        if (read(fd, &header.name_len, 1) != 1) {
            perror("Error reading gzip header");
            exit(1);
        }
        header.name = (unsigned char *)malloc(header.name_len);
        if (read(fd, header.name, header.name_len) != header.name_len) {
            perror("Error reading gzip header");
            exit(1);
        }
    }
    if (header.flg & 16) {
        if (read(fd, &header.comment_len, 1) != 1) {
            perror("Error reading gzip header");
            exit(1);
        }
        header.comment = (unsigned char *)malloc(header.comment_len);
        if (read(fd, header.comment, header.comment_len) != header.comment_len) {
            perror("Error reading gzip header");
            exit(1);
        }
    }
    return header;
}


int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <gzip_file>\n", argv[0]);
        return 1;
    }

    int fd = open(argv[1], O_RDONLY);
    if (fd == -1) {
        perror("Error opening file");
        return 1;
    }

    gzip_header header = read_gzip_header(fd);

    //Further processing of the gzip file can be added here.  For example,
    //inflate the compressed data using zlib.

    close(fd);
    return 0;
}
