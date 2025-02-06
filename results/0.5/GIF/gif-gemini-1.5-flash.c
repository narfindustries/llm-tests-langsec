#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef struct {
    int success;
    size_t offset;
    char error[256];
} HParserResult;

// Helper function to read a specific number of bytes
static HParserResult read_bytes(const HParser *p, size_t n, void *buf) {
    HParserResult result;
    HParseResult pr = h_parse(h_take(n), p->input + p->offset, n);
    if (pr.success) {
        memcpy(buf, pr.input, n);
        result.success = 1;
        result.offset = p->offset + n;
        strcpy(result.error, "");
    } else {
        result.success = 0;
        result.offset = p->offset + pr.offset;
        strncpy(result.error, pr.error, sizeof(result.error) - 1);
        result.error[sizeof(result.error) - 1] = '\0';
    }
    return result;
}

// Helper function to read an unsigned 16-bit integer in little-endian format
static HParser h_uint16_le() {
    return h_map(h_take(2), (HMapFunc)
                 (void* buf) {
                     return (uint16_t)((uint8_t*)buf)[0] | ((uint16_t)((uint8_t*)buf)[1] << 8);
                 });
}

// Helper function to read an unsigned 8-bit integer
static HParser h_uint8() {
    return h_map(h_take(1), (HMapFunc)
                 (void* buf) {
                     return (uint8_t)((uint8_t*)buf)[0];
                 });
}

// Parses the GIF header
static HParser parse_gif_header() {
    return h_seq(
        h_string("GIF89a"),
        h_uint16_le(), // Logical Screen Width
        h_uint16_le(), // Logical Screen Height
        h_uint8(), // Packed Fields
        h_uint8(), // Background Color Index
        h_uint8(), // Pixel Aspect Ratio
        h_end()
    );
}

// Parses the Global Color Table (GCT)
static HParser parse_gct(uint8_t gct_size) {
    size_t num_entries = 1 << (gct_size + 1);
    size_t size = num_entries * 3;
    return h_take(size);
}

// Parses a single image descriptor
static HParser parse_image_descriptor() {
    return h_seq(
        h_uint16_le(), // Image Left Position
        h_uint16_le(), // Image Top Position
        h_uint16_le(), // Image Width
        h_uint16_le(), // Image Height
        h_uint8(), // Packed Fields
        h_end()
    );
}

// Parses the Local Color Table (LCT)
static HParser parse_lct(uint8_t lct_size) {
    size_t num_entries = 1 << (lct_size + 1);
    size_t size = num_entries * 3;
    return h_take(size);
}

// Placeholder for image data parsing (LZW compression handling needed here)
static HParser parse_image_data() {
    return h_many(h_uint8());
}

// Parses GIF extension blocks (simplified - doesn't handle all extension types)
static HParser parse_extension_block() {
    return h_choice(
        h_string("\0"), //End of Block
        h_many(h_uint8()) //Generic Extension Data
    );
}

// Main GIF parser
static HParser parse_gif() {
    return h_seq(
        parse_gif_header(),
        h_bind(h_uint8(), (HBindFunc)
               (void* packed_fields, const HParser *p) {
                   uint8_t gct_size = (uint8_t)(((uint8_t*)packed_fields)[0] & 0x07);
                   uint8_t gct_flag = (uint8_t)(((uint8_t*)packed_fields)[0] >> 7);
                   HParser gct_parser = gct_flag ? parse_gct(gct_size) : h_empty();
                   return h_seq(gct_parser, h_many1(parse_image_descriptor), h_many(parse_extension_block), h_end(), h_empty());
               }),
        h_end()
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <gif_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    char *buffer = (char *)malloc(fsize);
    fread(buffer, 1, fsize, fp);
    fclose(fp);

    HParser parser = {buffer, 0, NULL}; // Initialize HParser struct correctly
    HParserResult result = read_bytes(&parser, fsize, buffer);
    HParseResult parse_result = h_parse(parse_gif(), buffer, fsize);

    if (parse_result.success) {
        printf("GIF parsed successfully!\n");
    } else {
        fprintf(stderr, "GIF parsing failed at offset %zu: %s\n", parse_result.offset, parse_result.error);
    }
    free(buffer);
    return 0;
}
