#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#define HAMMER_PARSER(name) struct HParser *name

struct HParser *jpeg = NULL;

void init_jpeg_parser() {
    jpeg = h_sequence(
        h_byte(0xFF),
        h_byte(0xD8),
        h_optional(
            h_sequence(
                h_byte(0xFF),
                h_byte(0xE0),
                h_uint16_be(),
                h_string("JFIF", 4),
                h_byte(),
                h_byte(),
                h_uint16_be(),
                h_uint16_be(),
                h_byte(),
                h_byte()
            )
        ),
        h_zero_or_more(
            h_sequence(
                h_byte(0xFF),
                h_byte(0xE1),
                h_uint16_be(),
                h_string("Exif", 4),
                h_any()
            )
        ),
        h_optional(
            h_sequence(
                h_byte(0xFF),
                h_byte(0xDB),
                h_uint16_be(),
                h_byte(),
                h_byte(),
                h_array(64, h_byte())
            )
        ),
        h_zero_or_more(
            h_sequence(
                h_byte(0xFF),
                h_byte(0xC4),
                h_uint16_be(),
                h_byte(),
                h_byte(),
                h_uint16_be(),
                h_array(162, h_byte())
            )
        ),
        h_sequence(
            h_byte(0xFF),
            h_byte(0xC0),
            h_uint16_be(),
            h_byte(),
            h_uint16_be(),
            h_uint16_be(),
            h_byte(),
            h_array(3, h_sequence(
                h_byte(),
                h_byte(),
                h_byte()
            ))
        ),
        h_optional(
            h_sequence(
                h_byte(0xFF),
                h_byte(0xC2),
                h_uint16_be(),
                h_byte(),
                h_uint16_be(),
                h_uint16_be(),
                h_byte(),
                h_array(3, h_sequence(
                    h_byte(),
                    h_byte(),
                    h_byte()
                ))
            )
        ),
        h_optional(
            h_sequence(
                h_byte(0xFF),
                h_byte(0xDD),
                h_uint16_be(),
                h_uint16_be()
            )
        ),
        h_sequence(
            h_byte(0xFF),
            h_byte(0xDA),
            h_uint16_be(),
            h_byte(),
            h_array(3, h_sequence(
                h_byte(),
                h_byte(),
                h_byte()
            )),
            h_any()
        ),
        h_byte(0xFF),
        h_byte(0xD9)
    );
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    init_jpeg_parser();

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        fprintf(stderr, "Error opening file %s\n", argv[1]);
        return 1;
    }

    struct HParseResult {
        int status;
    } result;
    result.status = 0;

    size_t size = 0;
    uint8_t *data = NULL;

    fseek(file, 0, SEEK_END);
    size = ftell(file);
    rewind(file);

    data = malloc(size);
    if (!data) {
        fprintf(stderr, "Error allocating memory\n");
        fclose(file);
        return 1;
    }

    if (fread(data, 1, size, file) != size) {
        fprintf(stderr, "Error reading file\n");
        free(data);
        fclose(file);
        return 1;
    }

    struct HParseResult *res = h_parse(jpeg, data, size);
    if (res->status != 0) {
        fprintf(stderr, "Error parsing file\n");
        free(data);
        fclose(file);
        return 1;
    }

    free(data);
    fclose(file);
    return 0;
}