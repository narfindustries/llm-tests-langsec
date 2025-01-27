#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

static HParser *tiff_header(void);
static HParser *tiff_ifd_entry(void);
static HParser *tiff_ifd(void);
static HParser *tiff_image_file(void);

static HParser *tiff_header() {
    HParser *endian_indicator = h_choice(h_uint16_le(), h_uint16_be(), NULL);
    HParser *magic_number = h_token((const uint8_t *)"\x2A\x00", 2);
    HParser *offset_to_first_ifd = h_choice(h_uint32_le(), h_uint32_be(), NULL);
    return h_sequence(endian_indicator, magic_number, offset_to_first_ifd, NULL);
}

static HParser *tiff_ifd_entry() {
    HParser *tag_id = h_choice(h_uint16_le(), h_uint16_be(), NULL);
    HParser *data_type = h_choice(h_uint16_le(), h_uint16_be(), NULL);
    HParser *count = h_choice(h_uint32_le(), h_uint32_be(), NULL);
    HParser *value_offset = h_choice(h_uint32_le(), h_uint32_be(), NULL);
    return h_sequence(tag_id, data_type, count, value_offset, NULL);
}

static HParser *tiff_ifd() {
    HParser *num_of_entries = h_choice(h_uint16_le(), h_uint16_be(), NULL);
    HParser *entries = h_repeat(tiff_ifd_entry(), num_of_entries);
    HParser *next_ifd_offset = h_choice(h_uint32_le(), h_uint32_be(), NULL);
    return h_sequence(num_of_entries, entries, next_ifd_offset, NULL);
}

static HParser *tiff_image_file() {
    HParser *header = tiff_header();
    HParser *ifd = tiff_ifd();
    return h_sequence(header, ifd, NULL);
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <tiff-file>\n", argv[0]);
        return 1;
    }

    const char *filepath = argv[1];
    FILE *file = fopen(filepath, "rb");
    if (!file) {
        perror("fopen");
        return 1;
    }
    
    fseek(file, 0, SEEK_END);
    long filesize = ftell(file);
    if (filesize < 0) {
        perror("ftell");
        fclose(file);
        return 1;
    }
    fseek(file, 0, SEEK_SET);
    
    uint8_t *buffer = (uint8_t *)malloc(filesize);
    if (!buffer) {
        perror("malloc");
        fclose(file);
        return 1;
    }
    
    if (fread(buffer, 1, filesize, file) != (size_t)filesize) {
        perror("fread");
        free(buffer);
        fclose(file);
        return 1;
    }
    fclose(file);

    HParser *tiff_parser = tiff_image_file();
    h_arena_t *arena = h_arena_create(1024);
    HParseResult *result = h_parse(tiff_parser, buffer, filesize, arena);
    
    if (result) {
        printf("TIFF file parsed successfully.\n");
    } else {
        printf("Failed to parse TIFF file.\n");
    }
    
    h_delete(tiff_parser);
    h_arena_destroy(arena);
    free(buffer);
    return 0;
}