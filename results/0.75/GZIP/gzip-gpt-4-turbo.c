#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// GZIP Magic numbers
#define GZIP_ID1 0x1F
#define GZIP_ID2 0x8B

// Compression method
#define CM_DEFLATE 8

// Flags
#define FTEXT    0x01
#define FHCRC    0x02
#define FEXTRA   0x04
#define FNAME    0x08
#define FCOMMENT 0x10

// OS types
enum OS {
    OS_FAT = 0,
    OS_AMIGA,
    OS_VMS,
    OS_UNIX,
    OS_VM_CMS,
    OS_ATARI_TOS,
    OS_HPFS,
    OS_MACINTOSH,
    OS_Z_SYSTEM,
    OS_CP_M,
    OS_TOPS_20,
    OS_NTFS,
    OS_QDOS,
    OS_ACORN_RISCOS,
    OS_UNKNOWN = 255
};

// Function to create a parser for the optional zero-terminated string
HParser *optional_string() {
    return h_sequence(h_many1(h_not(h_ch(0))), h_ch(0), NULL);
}

// Function to create a parser for the GZIP header
HParser *gzip_header() {
    HParser *id1 = h_ch(GZIP_ID1);
    HParser *id2 = h_ch(GZIP_ID2);
    HParser *cm = h_ch(CM_DEFLATE);
    HParser *flg = h_uint8();
    HParser *mtime = h_uint32();
    HParser *xfl = h_uint8();
    HParser *os = h_uint8();
    HParser *fextra = h_optional(h_length_value(h_uint16(), h_repeat_n(h_any(), h_uint16())));
    HParser *fname = optional_string();
    HParser *fcomment = optional_string();
    HParser *fhcrc = h_optional(h_uint16());

    return h_sequence(id1, id2, cm, flg, mtime, xfl, os, fextra, fname, fcomment, fhcrc, NULL);
}

// Main function
int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <GZIP File>\n", argv[0]);
        return EXIT_FAILURE;
    }

    // Open the GZIP file
    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("File opening failed");
        return EXIT_FAILURE;
    }

    // Read the file into memory
    fseek(fp, 0, SEEK_END);
    size_t fsize = ftell(fp);
    rewind(fp);
    uint8_t *buf = malloc(fsize);
    if (!buf) {
        fclose(fp);
        perror("Memory allocation failed");
        return EXIT_FAILURE;
    }
    fread(buf, fsize, 1, fp);
    fclose(fp);

    // Parse the buffer
    HParser *header_parser = gzip_header();
    HParseResult *result = h_parse(header_parser, buf, fsize);
    if (result) {
        printf("GZIP header parsed successfully.\n");
        h_pprint(stdout, result->ast, 0, 0);
    } else {
        fprintf(stderr, "Failed to parse GZIP header.\n");
    }

    // Cleanup
    h_parse_result_free(result);
    free(buf);

    return result ? EXIT_SUCCESS : EXIT_FAILURE;
}