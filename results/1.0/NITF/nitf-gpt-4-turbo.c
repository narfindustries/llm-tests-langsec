#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>
#include <string.h>

// Parser declarations for various components
HParser *nitf_version;
HParser *complexity_level;
HParser *standard_type;
HParser *orig_station_id;
HParser *file_datetime;
HParser *file_title;
HParser *classification;
HParser *encrypt;
HParser *fbkgc;
HParser *oname;
HParser *ophone;
HParser *file_length;
HParser *header_length;

HParser *image_identifier;
HParser *image_date_time;
HParser *target_id;
HParser *image_source;
HParser *num_rows;
HParser *num_cols;
HParser *pixel_value_type;
HParser *image_repr;
HParser *image_cat;
HParser *actual_bits_per_pixel;
HParser *image_geolo;
HParser *image_compression;
HParser *encryption;

uint8_t *read_entire_file(const char *filename, size_t *length) {
    FILE *f = fopen(filename, "rb");
    if (!f) {
        perror("Unable to open file");
        exit(EXIT_FAILURE);
    }

    fseek(f, 0, SEEK_END);
    *length = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t *data = malloc(*length);
    if (!data) {
        perror("Unable to allocate memory");
        fclose(f);
        exit(EXIT_FAILURE);
    }

    if (fread(data, 1, *length, f) != *length) {
        perror("Unable to read file");
        free(data);
        fclose(f);
        exit(EXIT_FAILURE);
    }

    fclose(f);
    return data;
}

void init_parsers() {
    nitf_version = h_sequence(h_ch('N'), h_ch('I'), h_ch('T'), h_ch('F'), h_ch('0'), h_ch('2'), h_ch('.'), h_ch('1'), h_ch('0'), NULL);
    complexity_level = h_int_range(h_uint8(), 1, 7);
    standard_type = h_sequence(h_ch('B'), h_ch('F'), h_ch('0'), h_ch('1'), NULL);
    orig_station_id = h_repeat_n(h_bit_range('0', '9', 1), 10);
    file_datetime = h_repeat_n(h_bit_range('0', '9', 1), 14);
    file_title = h_repeat_n(h_any(), 80);
    classification = h_int_range(h_uint8(), 1, 7);
    encrypt = h_int_range(h_uint8(), 0, 1);
    fbkgc = h_int_range(h_uint8(), 0, 255);
    oname = h_repeat_n(h_any(), 24);
    ophone = h_repeat_n(h_any(), 18);
    file_length = h_uint64();
    header_length = h_uint32();

    image_identifier = h_repeat_n(h_any(), 10);
    image_date_time = h_repeat_n(h_any(), 14);
    target_id = h_repeat_n(h_any(), 17);
    image_source = h_repeat_n(h_any(), 42);
    num_rows = h_uint32();
    num_cols = h_uint32();
    pixel_value_type = h_int_range(h_uint8(), 0, 3);
    image_repr = h_repeat_n(h_any(), 8);
    image_cat = h_repeat_n(h_any(), 8);
    actual_bits_per_pixel = h_int_range(h_uint8(), 0, 8);
    image_geolo = h_repeat_n(h_any(), 60);
    image_compression = h_int_range(h_uint8(), 0, 1);
    encryption = h_int_range(h_uint8(), 0, 1);
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <nitf_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    size_t length = 0;
    uint8_t *data = read_entire_file(argv[1], &length);

    init_parsers();

    HParseResult *result = h_parse(nitf_version, data, length);
    if (!result) {
        fprintf(stderr, "Parsing failed!\n");
        free(data);
        return EXIT_FAILURE;
    }

    printf("Parsing succeeded.\n");

    free(data);
    return EXIT_SUCCESS;
}