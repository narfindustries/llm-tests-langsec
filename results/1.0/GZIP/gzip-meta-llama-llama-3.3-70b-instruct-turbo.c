#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// Define constants
#define MAGIC_GZIP 0x8b1f
#define METHOD_DEFLATE 8
#define FLAGS_FEXTRA 0x04
#define FLAGS_FNAME 0x08
#define FLAGS_FCOMMENT 0x10

// Define the gzip file format structure
typedef struct {
    uint16_t magic;
    uint8_t method;
    uint8_t flags;
    uint32_t mtime;
    uint8_t xflags;
    uint8_t os;
} gzip_header_t;

typedef struct {
    uint16_t len;
} gzip_extra_t;

typedef struct {
    char *data;
    uint16_t len;
} gzip_filename_t;

typedef struct {
    char *data;
    uint16_t len;
} gzip_comment_t;

typedef struct {
    uint32_t crc;
    uint32_t isize;
} gzip_footer_t;

// Gzip file format specification
int main() {
    // Open the input file
    FILE *input_file = fopen("input.gz", "rb");
    if (input_file == NULL) {
        printf("Error opening input file\n");
        return 1;
    }

    // Read the gzip header
    gzip_header_t header;
    fread(&header, sizeof(header), 1, input_file);

    // Check the magic number
    if (header.magic != MAGIC_GZIP) {
        printf("Invalid gzip file\n");
        return 1;
    }

    // Check the compression method
    if (header.method != METHOD_DEFLATE) {
        printf("Unsupported compression method\n");
        return 1;
    }

    // Read extra fields if present
    if (header.flags & FLAGS_FEXTRA) {
        gzip_extra_t extra;
        fread(&extra, sizeof(extra), 1, input_file);
        // Skip extra data
        fseek(input_file, extra.len, SEEK_CUR);
    }

    // Read filename if present
    if (header.flags & FLAGS_FNAME) {
        gzip_filename_t filename;
        filename.len = 0;
        filename.data = NULL;
        int c;
        while ((c = fgetc(input_file)) != '\0') {
            filename.len++;
            filename.data = realloc(filename.data, filename.len + 1);
            filename.data[filename.len - 1] = c;
        }
        filename.data[filename.len] = '\0';
        // Print the filename
        printf("%s\n", filename.data);
        free(filename.data);
    }

    // Read comment if present
    if (header.flags & FLAGS_FCOMMENT) {
        gzip_comment_t comment;
        comment.len = 0;
        comment.data = NULL;
        int c;
        while ((c = fgetc(input_file)) != '\0') {
            comment.len++;
            comment.data = realloc(comment.data, comment.len + 1);
            comment.data[comment.len - 1] = c;
        }
        comment.data[comment.len] = '\0';
        // Print the comment
        printf("%s\n", comment.data);
        free(comment.data);
    }

    // Read the compressed data
    char *data = NULL;
    size_t data_len = 0;
    int c;
    while ((c = fgetc(input_file)) != EOF) {
        data_len++;
        data = realloc(data, data_len);
        data[data_len - 1] = c;
    }

    // Read the gzip footer
    gzip_footer_t footer;
    fread(&footer, sizeof(footer), 1, input_file);

    // Close the input file
    fclose(input_file);

    return 0;
}