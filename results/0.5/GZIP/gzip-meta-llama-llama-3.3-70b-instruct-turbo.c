#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

#define ID1 0x1F
#define ID2 0x8B
#define CM 8

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
} gzip_names_t;

typedef struct {
    uint16_t fhcrc;
} gzip_fhcrc_t;

typedef struct {
    uint8_t* compr_data;
    uint32_t isize;
} gzip_compr_data_t;

typedef struct hammer_parser {
    uint8_t* data;
    size_t size;
    size_t pos;
    int error;
    char* error_message;
} hammer_parser_t;

hammer_parser_t* hammer_new_parser(uint8_t* data, size_t size) {
    hammer_parser_t* p = malloc(sizeof(hammer_parser_t));
    p->data = data;
    p->size = size;
    p->pos = 0;
    p->error = 0;
    p->error_message = NULL;
    return p;
}

void hammer_free_parser(hammer_parser_t* p) {
    free(p);
}

uint8_t hammer_get_byte(hammer_parser_t* p) {
    if (p->pos >= p->size) {
        p->error = 1;
        p->error_message = "End of data reached";
        return 0;
    }
    return p->data[p->pos++];
}

uint16_t hammer_get_uint16_le(hammer_parser_t* p) {
    if (p->pos + 2 > p->size) {
        p->error = 1;
        p->error_message = "End of data reached";
        return 0;
    }
    uint16_t val = p->data[p->pos] | (p->data[p->pos + 1] << 8);
    p->pos += 2;
    return val;
}

uint32_t hammer_get_uint32_le(hammer_parser_t* p) {
    if (p->pos + 4 > p->size) {
        p->error = 1;
        p->error_message = "End of data reached";
        return 0;
    }
    uint32_t val = p->data[p->pos] | (p->data[p->pos + 1] << 8) | (p->data[p->pos + 2] << 16) | (p->data[p->pos + 3] << 24);
    p->pos += 4;
    return val;
}

uint8_t* hammer_get_bytes(hammer_parser_t* p, size_t len) {
    if (p->pos + len > p->size) {
        p->error = 1;
        p->error_message = "End of data reached";
        return NULL;
    }
    uint8_t* bytes = malloc(len);
    memcpy(bytes, p->data + p->pos, len);
    p->pos += len;
    return bytes;
}

uint8_t* hammer_get_null_terminated_string(hammer_parser_t* p) {
    size_t len = 0;
    while (p->pos + len < p->size && p->data[p->pos + len] != 0) {
        len++;
    }
    if (p->pos + len >= p->size) {
        p->error = 1;
        p->error_message = "End of data reached";
        return NULL;
    }
    uint8_t* str = malloc(len + 1);
    memcpy(str, p->data + p->pos, len);
    str[len] = 0;
    p->pos += len + 1;
    return str;
}

uint8_t* hammer_get_remaining_bytes(hammer_parser_t* p) {
    size_t len = p->size - p->pos;
    uint8_t* bytes = malloc(len);
    memcpy(bytes, p->data + p->pos, len);
    p->pos = p->size;
    return bytes;
}

hammer_parser_t* gzip_header(hammer_parser_t* p) {
    uint8_t id1 = hammer_get_byte(p);
    if (id1 != ID1) {
        p->error = 1;
        p->error_message = "Invalid ID1";
        return p;
    }
    uint8_t id2 = hammer_get_byte(p);
    if (id2 != ID2) {
        p->error = 1;
        p->error_message = "Invalid ID2";
        return p;
    }
    uint8_t cm = hammer_get_byte(p);
    if (cm != CM) {
        p->error = 1;
        p->error_message = "Invalid compression method";
        return p;
    }
    return p;
}

hammer_parser_t* gzip_flags(hammer_parser_t* p, uint8_t* flags) {
    *flags = hammer_get_byte(p);
    return p;
}

hammer_parser_t* gzip_mtime(hammer_parser_t* p, uint32_t* mtime) {
    *mtime = hammer_get_uint32_le(p);
    return p;
}

hammer_parser_t* gzip_xfl(hammer_parser_t* p, uint8_t* xfl) {
    *xfl = hammer_get_byte(p);
    return p;
}

hammer_parser_t* gzip_os(hammer_parser_t* p, uint8_t* os) {
    *os = hammer_get_byte(p);
    return p;
}

hammer_parser_t* gzip_extra(hammer_parser_t* p, gzip_extra_t* extra) {
    extra->xlen = hammer_get_uint16_le(p);
    extra->extra = hammer_get_bytes(p, extra->xlen);
    return p;
}

hammer_parser_t* gzip_fname(hammer_parser_t* p, uint8_t** fname) {
    *fname = hammer_get_null_terminated_string(p);
    return p;
}

hammer_parser_t* gzip_fcomment(hammer_parser_t* p, uint8_t** fcomment) {
    *fcomment = hammer_get_null_terminated_string(p);
    return p;
}

hammer_parser_t* gzip_fhcrc(hammer_parser_t* p, gzip_fhcrc_t* fhcrc) {
    fhcrc->fhcrc = hammer_get_uint16_le(p);
    return p;
}

hammer_parser_t* gzip_compr_data(hammer_parser_t* p, gzip_compr_data_t* compr_data) {
    compr_data->compr_data = hammer_get_remaining_bytes(p);
    return p;
}

hammer_parser_t* gzip_isize(hammer_parser_t* p, uint32_t* isize) {
    *isize = hammer_get_uint32_le(p);
    return p;
}

hammer_parser_t* gzip_parser(hammer_parser_t* p) {
    gzip_header_t header;
    gzip_extra_t extra;
    gzip_names_t names;
    gzip_fhcrc_t fhcrc;
    gzip_compr_data_t compr_data;

    p = gzip_header(p);
    p = gzip_flags(p, &header.flags);
    p = gzip_mtime(p, &header.mtime);
    p = gzip_xfl(p, &header.xfl);
    p = gzip_os(p, &header.os);

    if (header.flags & 0x04) {
        p = gzip_extra(p, &extra);
    }

    if (header.flags & 0x08) {
        p = gzip_fname(p, &names.fname);
    }

    if (header.flags & 0x10) {
        p = gzip_fcomment(p, &names.fcomment);
    }

    if (header.flags & 0x02) {
        p = gzip_fhcrc(p, &fhcrc);
    }

    p = gzip_compr_data(p, &compr_data);
    p = gzip_isize(p, &compr_data.isize);

    return p;
}

int main(int argc, char** argv) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* data = malloc(file_size);
    fread(data, 1, file_size, file);
    fclose(file);

    hammer_parser_t* p = hammer_new_parser(data, file_size);
    p = gzip_parser(p);

    if (p->error) {
        printf("Error parsing gzip file: %s\n", p->error_message);
        return 1;
    }

    free(data);
    hammer_free_parser(p);

    return 0;
}