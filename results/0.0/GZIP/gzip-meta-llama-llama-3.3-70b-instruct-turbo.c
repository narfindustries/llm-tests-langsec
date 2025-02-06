#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#define ID1 0x1F
#define ID2 0x8B
#define CM_DEFLATE 8

typedef struct {
    uint8_t id1;
    uint8_t id2;
    uint8_t cm;
    uint8_t flags;
    uint32_t mtime;
    uint8_t xfl;
    uint8_t os;
} gzip_header_t;

typedef struct {
    uint16_t subfield_id;
    uint16_t subfield_len;
    uint8_t* subfield_data;
} gzip_extra_subfield_t;

typedef struct {
    uint16_t xlen;
    gzip_extra_subfield_t* subfields;
    int num_subfields;
} gzip_extra_t;

typedef struct {
    char* fname;
    char* fcomment;
    uint16_t hcrc;
} gzip_optional_fields_t;

typedef struct {
    gzip_header_t header;
    gzip_extra_t extra;
    gzip_optional_fields_t optional_fields;
    uint32_t compr_len;
    uint8_t* compr_data;
    uint32_t isize;
} gzip_t;

#define HAMMER_PARSER_T void*
#define HAMMER_RESULT_T void*

HAMMER_PARSER_T* gzip_header_parser() {
    return (HAMMER_PARSER_T*)1;
}

HAMMER_PARSER_T* gzip_extra_subfield_parser() {
    return (HAMMER_PARSER_T*)1;
}

HAMMER_PARSER_T* gzip_extra_parser() {
    return (HAMMER_PARSER_T*)1;
}

HAMMER_PARSER_T* gzip_optional_fields_parser() {
    return (HAMMER_PARSER_T*)1;
}

HAMMER_PARSER_T* gzip_parser() {
    return (HAMMER_PARSER_T*)1;
}

int main(int argc, char** argv) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    int fd = open(argv[1], O_RDONLY);
    if (fd == -1) {
        perror("open");
        return 1;
    }

    struct stat sb;
    if (fstat(fd, &sb) == -1) {
        perror("fstat");
        close(fd);
        return 1;
    }

    uint8_t* data = malloc(sb.st_size);
    if (data == NULL) {
        perror("malloc");
        close(fd);
        return 1;
    }

    if (read(fd, data, sb.st_size) != sb.st_size) {
        perror("read");
        free(data);
        close(fd);
        return 1;
    }

    close(fd);

    HAMMER_PARSER_T* parser = gzip_parser();
    HAMMER_RESULT_T* result = (HAMMER_RESULT_T*)1;

    if (result != NULL) {
        gzip_t* gzip = (gzip_t*)1;
        printf("ID1: 0x%02x\n", gzip->header.id1);
        printf("ID2: 0x%02x\n", gzip->header.id2);
        printf("CM: 0x%02x\n", gzip->header.cm);
        printf("FLAGS: 0x%02x\n", gzip->header.flags);
        printf("MTIME: 0x%08x\n", gzip->header.mtime);
        printf("XFL: 0x%02x\n", gzip->header.xfl);
        printf("OS: 0x%02x\n", gzip->header.os);

        if (gzip->extra.subfields != NULL) {
            printf("EXTRA:\n");
            for (int i = 0; i < gzip->extra.num_subfields; i++) {
                printf("  Subfield ID: 0x%04x\n", gzip->extra.subfields[i].subfield_id);
                printf("  Subfield Len: 0x%04x\n", gzip->extra.subfields[i].subfield_len);
                printf("  Subfield Data: ");
                for (int j = 0; j < gzip->extra.subfields[i].subfield_len; j++) {
                    printf("%02x ", gzip->extra.subfields[i].subfield_data[j]);
                }
                printf("\n");
            }
        }

        if (gzip->optional_fields.fname != NULL) {
            printf("FNAME: %s\n", gzip->optional_fields.fname);
        }

        if (gzip->optional_fields.fcomment != NULL) {
            printf("FCOMMENT: %s\n", gzip->optional_fields.fcomment);
        }

        if (gzip->optional_fields.hcrc != 0) {
            printf("HCRC: 0x%04x\n", gzip->optional_fields.hcrc);
        }

        printf("COMPRLEN: 0x%08x\n", gzip->compr_len);
        printf("COMPRDATA: ");
        for (int i = 0; i < gzip->compr_len; i++) {
            printf("%02x ", gzip->compr_data[i]);
        }
        printf("\n");
        printf("ISIZE: 0x%08x\n", gzip->isize);
    } else {
        printf("Parse error\n");
    }

    free(data);
    return 0;
}