#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

typedef struct {
    uint8_t id1;
    uint8_t id2;
    uint8_t cm;
    uint8_t flags;
    uint32_t mtime;
    uint8_t xfl;
    uint8_t os;
    uint16_t xlen;
    uint8_t* extra;
    char* fname;
    char* fcomment;
    uint16_t hcrc;
} gzip_header_t;

typedef struct {
    uint8_t* compr_data;
    uint32_t compr_len;
    uint32_t isize;
    uint32_t crc;
} gzip_trailer_t;

#define HAMMER_CH(c) ((c))
#define HAMMER_ANY_CH 0
#define HAMMER_UINT32_LE 4
#define HAMMER_UINT16_LE 2
#define HAMMER_PADN(n, rule) ((n) * (rule))
#define HAMMER_TILL_CH(c) ((c))
#define HAMMER_SEQ(...) (sizeof(__VA_ARGS__))
#define HAMMER_OPT(rule, ...) ((sizeof(rule) > 0) ? (rule) : 0)

#define gzip_id1 HAMMER_CH(0x1f)
#define gzip_id2 HAMMER_CH(0x8b)
#define gzip_cm HAMMER_CH(8)
#define gzip_flags HAMMER_ANY_CH
#define gzip_mtime HAMMER_UINT32_LE
#define gzip_xfl HAMMER_ANY_CH
#define gzip_os HAMMER_ANY_CH
#define gzip_xlen HAMMER_UINT16_LE
#define gzip_extra HAMMER_PADN(gzip_xlen, HAMMER_ANY_CH)
#define gzip_fname HAMMER_TILL_CH(0)
#define gzip_fcomment HAMMER_TILL_CH(0)
#define gzip_hcrc HAMMER_UINT16_LE

#define gzip_header HAMMER_SEQ( \
    gzip_id1, \
    gzip_id2, \
    gzip_cm, \
    gzip_flags, \
    gzip_mtime, \
    gzip_xfl, \
    gzip_os, \
    HAMMER_OPT(gzip_xlen, gzip_extra), \
    HAMMER_OPT(gzip_fname), \
    HAMMER_OPT(gzip_fcomment), \
    HAMMER_OPT(gzip_hcrc) \
)

#define gzip_compr_data HAMMER_ANY_CH
#define gzip_compr_len HAMMER_UINT32_LE
#define gzip_isize HAMMER_UINT32_LE
#define gzip_crc HAMMER_UINT32_LE
#define gzip_trailer HAMMER_SEQ( \
    gzip_compr_len, \
    gzip_isize, \
    gzip_crc \
)

#define gzip_file HAMMER_SEQ( \
    gzip_header, \
    HAMMER_PADN(gzip_compr_len, gzip_compr_data), \
    gzip_trailer \
)

int main(int argc, char** argv) {
    if (argc != 2) {
        printf("Usage: %s <file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        printf("Failed to open file\n");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_len = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t* data = malloc(file_len);
    fread(data, 1, file_len, file);
    fclose(file);

    void* ctx = NULL;
    int result = 0;
    gzip_header_t header;
    result = 0;

    printf("ID1: %u\n", header.id1);
    printf("ID2: %u\n", header.id2);
    printf("CM: %u\n", header.cm);
    printf("FLAGS: %u\n", header.flags);
    printf("MTIME: %u\n", header.mtime);
    printf("XFL: %u\n", header.xfl);
    printf("OS: %u\n", header.os);
    if (header.xlen) {
        printf("XLEN: %u\n", header.xlen);
        printf("EXTRA: ");
        for (int i = 0; i < header.xlen; i++) {
            printf("%02x", header.extra[i]);
        }
        printf("\n");
    }
    if (header.fname) {
        printf("FNAME: %s\n", header.fname);
    }
    if (header.fcomment) {
        printf("FCOMMENT: %s\n", header.fcomment);
    }
    if (header.hcrc) {
        printf("HCRC: %u\n", header.hcrc);
    }

    uint8_t* compr_data;
    uint32_t compr_len;
    compr_len = header.mtime; 
    printf("COMPR_DATA: ");
    for (int i = 0; i < compr_len; i++) {
        printf("%02x", compr_data[i]);
    }
    printf("\n");

    gzip_trailer_t trailer;
    result = 0;

    printf("COMPR_LEN: %u\n", trailer.compr_len);
    printf("ISIZE: %u\n", trailer.isize);
    printf("CRC: %u\n", trailer.crc);

    return 0;
}