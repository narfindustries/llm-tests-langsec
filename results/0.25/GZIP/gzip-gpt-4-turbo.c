#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define the GZIP header fields
#define ID1 0x1f
#define ID2 0x8b
#define CM_DEFLATE 0x08

// Flags
#define FTEXT 0x01
#define FHCRC 0x02
#define FEXTRA 0x04
#define FNAME 0x08
#define FCOMMENT 0x10

// OS types
enum {
    OS_FAT = 0x00,
    OS_AMIGA = 0x01,
    OS_VMS = 0x02,
    OS_UNIX = 0x03,
    OS_VM_CMS = 0x04,
    OS_ATARI_TOS = 0x05,
    OS_HPFS = 0x06,
    OS_MACINTOSH = 0x07,
    OS_Z_SYSTEM = 0x08,
    OS_CP_M = 0x09,
    OS_TOPS_20 = 0x0A,
    OS_NTFS = 0x0B,
    OS_QDOS = 0x0C,
    OS_ACORN_RISCOS = 0x0D,
    OS_UNKNOWN = 0xFF
};

// Hammer parsers for GZIP components
static HParser *gzip_header;
static HParser *gzip_member;
static HParser *gzip_file;

void init_parsers() {
    HParser *id1 = h_uint8();
    HParser *id2 = h_uint8();
    HParser *cm = h_uint8();
    HParser *flg = h_uint8();
    HParser *mtime = h_uint32();
    HParser *xfl = h_uint8();
    HParser *os = h_uint8();
    HParser *extra_len = h_uint16();
    HParser *extra_data = h_length_value(extra_len, h_uint8());
    HParser *fname = h_sequence(h_many(h_not(h_ch('\0'))), h_ch('\0'));
    HParser *fcomment = h_sequence(h_many(h_not(h_ch('\0'))), h_ch('\0'));
    HParser *fhcrc = h_uint16();
    HParser *crc32 = h_uint32();
    HParser *isize = h_uint32();

    gzip_header = h_sequence(id1, id2, cm, flg, mtime, xfl, os, NULL);

    HParser *optional_extra = h_optional(h_sequence(extra_len, extra_data));
    HParser *optional_fname = h_optional(fname);
    HParser *optional_fcomment = h_optional(fcomment);
    HParser *optional_fhcrc = h_optional(fhcrc);

    gzip_member = h_sequence(gzip_header, optional_extra, optional_fname, optional_fcomment, optional_fhcrc, crc32, isize, NULL);
    gzip_file = h_many(gzip_member);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <gzip_file>\n", argv[0]);
        return 1;
    }

    init_parsers();

    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("Failed to open file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    size_t size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *buffer = malloc(size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(fp);
        return 1;
    }

    if (fread(buffer, 1, size, fp) != size) {
        perror("Failed to read file");
        free(buffer);
        fclose(fp);
        return 1;
    }

    fclose(fp);

    HParseResult *result = h_parse(gzip_file, buffer, size);
    if (result) {
        printf("GZIP file parsed successfully.\n");
    } else {
        fprintf(stderr, "Failed to parse GZIP file.\n");
    }

    free(buffer);
    return 0;
}