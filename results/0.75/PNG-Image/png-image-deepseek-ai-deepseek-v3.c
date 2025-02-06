#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

#define PNG_SIGNATURE "\x89PNG\r\n\x1a\n"
#define PNG_SIGNATURE_LENGTH 8

HParser *png_parser() {
    HParser *signature = h_sequence(h_bytes(PNG_SIGNATURE_LENGTH, (const uint8_t*)PNG_SIGNATURE), NULL);

    HParser *chunk_length = h_uint32();
    HParser *chunk_type = h_bytes(4, NULL);
    HParser *chunk_data = h_length_value(h_uint32(), h_uint8());
    HParser *chunk_crc = h_uint32();

    HParser *chunk = h_sequence(chunk_length, chunk_type, chunk_data, chunk_crc, NULL);

    HParser *ihdr = h_sequence(h_uint32(), h_uint32(), h_uint8(), h_uint8(), h_uint8(), h_uint8(), h_uint8(), NULL);
    HParser *plte = h_repeat_n(h_bytes(3, NULL), h_uint32());
    HParser *idat = h_length_value(h_uint32(), h_uint8());
    HParser *iend = h_success(NULL);

    HParser *trns = h_choice(h_bytes(2, NULL), h_bytes(6, NULL), h_length_value(h_uint32(), h_uint8()), NULL);
    HParser *chrm = h_sequence(h_uint32(), h_uint32(), h_uint32(), h_uint32(), h_uint32(), h_uint32(), h_uint32(), h_uint32(), NULL);
    HParser *gama = h_uint32();
    HParser *iccp = h_sequence(h_length_value(h_uint32(), h_uint8()), h_uint8(), h_length_value(h_uint32(), h_uint8()), NULL);
    HParser *sbit = h_choice(h_bytes(1, NULL), h_bytes(3, NULL), h_bytes(4, NULL), NULL);
    HParser *srgb = h_uint8();
    HParser *text = h_sequence(h_length_value(h_uint32(), h_uint8()), h_length_value(h_uint32(), h_uint8()), NULL);
    HParser *ztxt = h_sequence(h_length_value(h_uint32(), h_uint8()), h_uint8(), h_length_value(h_uint32(), h_uint8()), NULL);
    HParser *itxt = h_sequence(h_length_value(h_uint32(), h_uint8()), h_uint8(), h_uint8(), h_length_value(h_uint32(), h_uint8()), h_length_value(h_uint32(), h_uint8()), NULL);
    HParser *bkgd = h_choice(h_bytes(1, NULL), h_bytes(2, NULL), h_bytes(6, NULL), NULL);
    HParser *hist = h_length_value(h_uint32(), h_uint16());
    HParser *phys = h_sequence(h_uint32(), h_uint32(), h_uint8(), NULL);
    HParser *splt = h_sequence(h_length_value(h_uint32(), h_uint8()), h_uint8(), h_length_value(h_uint32(), h_uint8()), NULL);
    HParser *time = h_sequence(h_uint16(), h_uint16(), h_uint16(), h_uint16(), h_uint16(), h_uint16(), NULL);

    HParser *chunks = h_repeat(chunk);

    return h_sequence(signature, chunks, NULL);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = (uint8_t *)malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *parser = png_parser();
    HParseResult *result = h_parse(parser, data, file_size);

    if (result) {
        printf("Parsing succeeded!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed.\n");
    }

    free(data);
    return 0;
}