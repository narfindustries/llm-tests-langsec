#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>

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
    uint16_t xlen;
    uint8_t* extra;
} gzip_extra_t;

typedef struct {
    uint8_t* fname;
    uint8_t* fcomment;
    uint16_t hcrc;
} gzip_optional_t;

typedef struct {
    gzip_header_t header;
    gzip_extra_t extra;
    gzip_optional_t optional;
    uint8_t* compressed_data;
    uint32_t isize;
} gzip_t;

typedef struct {
    uint8_t* data;
    size_t size;
    size_t pos;
} hammer_context_t;

void hammer_init(hammer_context_t* ctx, uint8_t* data, size_t size) {
    ctx->data = data;
    ctx->size = size;
    ctx->pos = 0;
}

void hammer_read_bytes(hammer_context_t* ctx, void* dest, size_t len) {
    if (ctx->pos + len > ctx->size) {
        printf("Error: Out of bounds\n");
        exit(1);
    }
    memcpy(dest, ctx->data + ctx->pos, len);
    ctx->pos += len;
}

void hammer_read_uint32_le(hammer_context_t* ctx, uint32_t* dest) {
    uint8_t bytes[4];
    hammer_read_bytes(ctx, bytes, 4);
    *dest = (uint32_t)bytes[0] | ((uint32_t)bytes[1] << 8) | ((uint32_t)bytes[2] << 16) | ((uint32_t)bytes[3] << 24);
}

void hammer_read_uint16_le(hammer_context_t* ctx, uint16_t* dest) {
    uint8_t bytes[2];
    hammer_read_bytes(ctx, bytes, 2);
    *dest = (uint16_t)bytes[0] | ((uint16_t)bytes[1] << 8);
}

void gzip_id1(hammer_context_t* ctx) {
    uint8_t id1 = ID1;
    hammer_read_bytes(ctx, &id1, 1);
}

void gzip_id2(hammer_context_t* ctx) {
    uint8_t id2 = ID2;
    hammer_read_bytes(ctx, &id2, 1);
}

void gzip_cm(hammer_context_t* ctx) {
    uint8_t cm = CM_DEFLATE;
    hammer_read_bytes(ctx, &cm, 1);
}

void gzip_flags(hammer_context_t* ctx, uint8_t* flags) {
    hammer_read_bytes(ctx, flags, 1);
}

void gzip_mtime(hammer_context_t* ctx, uint32_t* mtime) {
    hammer_read_uint32_le(ctx, mtime);
}

void gzip_xfl(hammer_context_t* ctx, uint8_t* xfl) {
    hammer_read_bytes(ctx, xfl, 1);
}

void gzip_os(hammer_context_t* ctx, uint8_t* os) {
    hammer_read_bytes(ctx, os, 1);
}

void gzip_extra_len(hammer_context_t* ctx, uint16_t* xlen) {
    hammer_read_uint16_le(ctx, xlen);
}

void gzip_extra(hammer_context_t* ctx, uint16_t xlen, uint8_t** extra) {
    *extra = malloc(xlen);
    hammer_read_bytes(ctx, *extra, xlen);
}

void gzip_fname(hammer_context_t* ctx, uint8_t** fname) {
    uint8_t c;
    size_t len = 0;
    *fname = NULL;
    while (1) {
        hammer_read_bytes(ctx, &c, 1);
        if (c == 0) break;
        *fname = realloc(*fname, len + 1);
        (*fname)[len++] = c;
    }
    (*fname)[len] = 0;
}

void gzip_fcomment(hammer_context_t* ctx, uint8_t** fcomment) {
    uint8_t c;
    size_t len = 0;
    *fcomment = NULL;
    while (1) {
        hammer_read_bytes(ctx, &c, 1);
        if (c == 0) break;
        *fcomment = realloc(*fcomment, len + 1);
        (*fcomment)[len++] = c;
    }
    (*fcomment)[len] = 0;
}

void gzip_hcrc(hammer_context_t* ctx, uint16_t* hcrc) {
    hammer_read_uint16_le(ctx, hcrc);
}

void gzip_optional(hammer_context_t* ctx, uint8_t flags, gzip_optional_t* optional, gzip_extra_t* extra) {
    if (flags & 0x04) {
        uint16_t xlen;
        gzip_extra_len(ctx, &xlen);
        gzip_extra(ctx, xlen, &extra->extra);
        extra->xlen = xlen;
    }
    if (flags & 0x08) {
        gzip_fname(ctx, &optional->fname);
    }
    if (flags & 0x10) {
        gzip_fcomment(ctx, &optional->fcomment);
    }
    if (flags & 0x02) {
        gzip_hcrc(ctx, &optional->hcrc);
    }
}

void gzip_header(hammer_context_t* ctx, gzip_header_t* header, gzip_optional_t* optional, gzip_extra_t* extra) {
    gzip_id1(ctx);
    gzip_id2(ctx);
    gzip_cm(ctx);
    gzip_flags(ctx, &header->flags);
    gzip_mtime(ctx, &header->mtime);
    gzip_xfl(ctx, &header->xfl);
    gzip_os(ctx, &header->os);
    gzip_optional(ctx, header->flags, optional, extra);
}

void gzip_compressed_data(hammer_context_t* ctx, uint8_t** compressed_data, uint32_t* isize) {
    size_t len = 0;
    *compressed_data = NULL;
    while (1) {
        uint8_t c;
        hammer_read_bytes(ctx, &c, 1);
        if (c == 0x00 && ctx->pos + 3 <= ctx->size && ctx->data[ctx->pos] == 0x00 && ctx->data[ctx->pos + 1] == 0x00 && ctx->data[ctx->pos + 2] == 0x00) break;
        *compressed_data = realloc(*compressed_data, len + 1);
        (*compressed_data)[len++] = c;
    }
    *isize = len;
    ctx->pos += 4;
}

void gzip_isize(hammer_context_t* ctx, uint32_t* isize) {
    hammer_read_uint32_le(ctx, isize);
}

void gzip_parse(hammer_context_t* ctx, gzip_t* gzip) {
    gzip_header_t header;
    gzip_optional_t optional;
    gzip_extra_t extra;
    gzip_header(ctx, &header, &optional, &extra);
    uint8_t* compressed_data;
    uint32_t isize;
    gzip_compressed_data(ctx, &compressed_data, &isize);
    gzip_isize(ctx, &isize);
    gzip->header = header;
    gzip->extra = extra;
    gzip->optional = optional;
    gzip->compressed_data = compressed_data;
    gzip->isize = isize;
}

int main(int argc, char** argv) {
    if (argc != 2) {
        printf("Usage: %s <file>\n", argv[0]);
        return 1;
    }

    int fd = open(argv[1], O_RDONLY);
    if (fd < 0) {
        perror("open");
        return 1;
    }

    size_t size = lseek(fd, 0, SEEK_END);
    if (size < 0) {
        perror("lseek");
        close(fd);
        return 1;
    }

    lseek(fd, 0, SEEK_SET);

    uint8_t* data = malloc(size);
    if (!data) {
        perror("malloc");
        close(fd);
        return 1;
    }

    ssize_t bytes_read = read(fd, data, size);
    if (bytes_read < 0) {
        perror("read");
        free(data);
        close(fd);
        return 1;
    }

    hammer_context_t ctx;
    hammer_init(&ctx, data, size);

    gzip_t gzip;
    gzip_parse(&ctx, &gzip);

    printf("ID1: 0x%02x\n", gzip.header.id1);
    printf("ID2: 0x%02x\n", gzip.header.id2);
    printf("CM: 0x%02x\n", gzip.header.cm);
    printf("FLAGS: 0x%02x\n", gzip.header.flags);
    printf("MTIME: 0x%08x\n", gzip.header.mtime);
    printf("XFL: 0x%02x\n", gzip.header.xfl);
    printf("OS: 0x%02x\n", gzip.header.os);

    if (gzip.header.flags & 0x04) {
        printf("EXTRA: ");
        for (size_t i = 0; i < gzip.extra.xlen; i++) {
            printf("%02x", gzip.extra.extra[i]);
        }
        printf("\n");
    }

    if (gzip.header.flags & 0x08) {
        printf("FNAME: %s\n", gzip.optional.fname);
    }

    if (gzip.header.flags & 0x10) {
        printf("FCOMMENT: %s\n", gzip.optional.fcomment);
    }

    if (gzip.header.flags & 0x02) {
        printf("HCRC: 0x%04x\n", gzip.optional.hcrc);
    }

    printf("COMpressed data: ");
    for (size_t i = 0; i < gzip.isize; i++) {
        printf("%02x", gzip.compressed_data[i]);
    }
    printf("\n");

    printf("ISIZE: 0x%08x\n", gzip.isize);

    free(data);
    close(fd);
    return 0;
}