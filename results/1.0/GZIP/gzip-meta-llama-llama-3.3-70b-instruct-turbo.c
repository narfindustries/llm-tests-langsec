#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef struct {
    uint8_t id1;
    uint8_t id2;
    uint8_t cm;
    uint8_t flags;
    uint32_t mtime;
    uint8_t xfl;
    uint8_t os;
    uint16_t xlen;
    char* xfield;
    char* fname;
    char* fcomment;
    uint16_t fhcrc;
    char* data;
    uint32_t isize;
} gzip_t;

#define CRCPOLY 0xedb88320

uint16_t crc16(uint8_t* bytes, size_t len, uint16_t crc) {
    int i, j;
    for (i = 0; i < len; i++) {
        crc ^= bytes[i];
        for (j = 0; j < 8; j++) {
            if (crc & 1) {
                crc = (crc >> 1) ^ CRCPOLY;
            } else {
                crc >>= 1;
            }
        }
    }
    return crc;
}

void gzip_parse(uint8_t* buffer, size_t size, gzip_t* gzip) {
    gzip->id1 = buffer[0];
    gzip->id2 = buffer[1];
    gzip->cm = buffer[2];
    gzip->flags = buffer[3];
    gzip->mtime = *(uint32_t*)(buffer + 4);
    gzip->xfl = buffer[8];
    gzip->os = buffer[9];

    size_t offset = 10;

    if (gzip->flags & 4) {
        gzip->xlen = *(uint16_t*)(buffer + offset);
        offset += 2;
        gzip->xfield = malloc(gzip->xlen);
        memcpy(gzip->xfield, buffer + offset, gzip->xlen);
        offset += gzip->xlen;
    } else {
        gzip->xlen = 0;
        gzip->xfield = NULL;
    }

    if (gzip->flags & 8) {
        size_t fname_len = 0;
        while (buffer[offset + fname_len] != 0) {
            fname_len++;
        }
        gzip->fname = malloc(fname_len + 1);
        memcpy(gzip->fname, buffer + offset, fname_len);
        gzip->fname[fname_len] = 0;
        offset += fname_len + 1;
    } else {
        gzip->fname = NULL;
    }

    if (gzip->flags & 16) {
        size_t fcomment_len = 0;
        while (buffer[offset + fcomment_len] != 0) {
            fcomment_len++;
        }
        gzip->fcomment = malloc(fcomment_len + 1);
        memcpy(gzip->fcomment, buffer + offset, fcomment_len);
        gzip->fcomment[fcomment_len] = 0;
        offset += fcomment_len + 1;
    } else {
        gzip->fcomment = NULL;
    }

    if (gzip->flags & 2) {
        gzip->fhcrc = *(uint16_t*)(buffer + offset);
        offset += 2;
    } else {
        gzip->fhcrc = 0;
    }

    gzip->data = malloc(size - offset - 4);
    memcpy(gzip->data, buffer + offset, size - offset - 4);
    gzip->isize = *(uint32_t*)(buffer + size - 4);
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        printf("Usage: %s <gzipfile>\n", argv[0]);
        return 1;
    }
    FILE* fp = fopen(argv[1], "rb");
    if (!fp) {
        printf("Failed to open %s\n", argv[1]);
        return 1;
    }
    fseek(fp, 0, SEEK_END);
    size_t size = ftell(fp);
    rewind(fp);
    uint8_t* buffer = malloc(size);
    if (fread(buffer, 1, size, fp) != size) {
        printf("Failed to read %s\n", argv[1]);
        fclose(fp);
        free(buffer);
        return 1;
    }
    fclose(fp);
    gzip_t gzip;
    gzip_parse(buffer, size, &gzip);
    if (gzip.id1 != 0x1f || gzip.id2 != 0x8b) {
        printf("Failed to parse %s\n", argv[1]);
        free(buffer);
        return 1;
    }
    printf("GZIP file:\n");
    printf("  ID1: 0x%02x\n", gzip.id1);
    printf("  ID2: 0x%02x\n", gzip.id2);
    printf("  CM: %u\n", gzip.cm);
    printf("  Flags: 0x%02x\n", gzip.flags);
    printf("  MTIME: %u\n", gzip.mtime);
    printf("  XFL: %u\n", gzip.xfl);
    printf("  OS: %u\n", gzip.os);
    if (gzip.flags & 4) {
        printf("  XLEN: %u\n", gzip.xlen);
        printf("  XFIELD: %.*s\n", gzip.xlen, gzip.xfield);
    }
    if (gzip.flags & 8) {
        printf("  FNAME: %s\n", gzip.fname);
    }
    if (gzip.flags & 16) {
        printf("  FCOMMENT: %s\n", gzip.fcomment);
    }
    if (gzip.flags & 2) {
        printf("  FHCRC: 0x%04x\n", gzip.fhcrc);
        uint16_t crc = crc16(buffer, 10 + (gzip.flags & 4 ? 2 + gzip.xlen : 0) + (gzip.flags & 8 ? strlen(gzip.fname) + 1 : 0) + (gzip.flags & 16 ? strlen(gzip.fcomment) + 1 : 0), 0);
        if (crc != gzip.fhcrc) {
            printf("  Warning: CRC mismatch\n");
        }
    }
    printf("  Compressed data: %.*s\n", (int)(size - 10 - (gzip.flags & 4 ? 2 + gzip.xlen : 0) - (gzip.flags & 8 ? strlen(gzip.fname) + 1 : 0) - (gzip.flags & 16 ? strlen(gzip.fcomment) + 1 : 0) - (gzip.flags & 2 ? 2 : 0) - 4), gzip.data);
    printf("  ISIZE: %u\n", gzip.isize);
    free(buffer);
    if (gzip.xfield) free(gzip.xfield);
    if (gzip.fname) free(gzip.fname);
    if (gzip.fcomment) free(gzip.fcomment);
    if (gzip.data) free(gzip.data);
    return 0;
}