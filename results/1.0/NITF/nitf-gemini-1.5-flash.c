#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <hammer/hammer.h>

typedef struct {
    uint32_t segment_length;
    char segment_identifier[5];
    uint16_t version;
    uint32_t offset_to_imagedata;
    uint32_t image_width;
    uint32_t image_height;
} nitf_header_t;

hm_parser_t* uint32_parser() {
    return hm_uint32_le();
}

hm_parser_t* uint16_parser() {
    return hm_uint16_le();
}

hm_parser_t* string_parser(size_t len) {
    return hm_string(len);
}

hm_parser_t* nitf_header_parser() {
    return hm_map(
        hm_tuple(
            uint32_parser(),
            string_parser(4),
            uint16_parser(),
            uint32_parser(),
            uint32_parser(),
            uint32_parser()
        ),
        (hm_map_func_t) [](const hm_value_t* tuple) {
            nitf_header_t* header = malloc(sizeof(nitf_header_t));
            if (header == NULL) return hm_err("Memory allocation failed");
            header->segment_length = hm_get_uint32(hm_get_tuple_element(tuple, 0));
            strcpy(header->segment_identifier, hm_get_string(hm_get_tuple_element(tuple, 1)));
            header->version = hm_get_uint16(hm_get_tuple_element(tuple, 2));
            header->offset_to_imagedata = hm_get_uint32(hm_get_tuple_element(tuple, 3));
            header->image_width = hm_get_uint32(hm_get_tuple_element(tuple, 4));
            header->image_height = hm_get_uint32(hm_get_tuple_element(tuple, 5));
            return hm_ok(header);
        }
    );
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <nitf_file>\n", argv[0]);
        return 1;
    }

    FILE* fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fileSize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    hm_parser_t* parser = nitf_header_parser();
    hm_reader_t* reader = hm_file_reader(fp, fileSize);
    hm_result_t result = hm_parse(parser, reader);

    if (result.is_err) {
        fprintf(stderr, "Parsing failed: %s\n", result.error_msg);
        fclose(fp);
        hm_reader_free(reader);
        hm_parser_free(parser);
        return 1;
    }

    nitf_header_t* header = result.value;
    printf("NITF Header:\n");
    printf("  Segment Length: %u\n", header->segment_length);
    printf("  Segment Identifier: %s\n", header->segment_identifier);
    printf("  Version: %u\n", header->version);
    printf("  Offset to Image Data: %u\n", header->offset_to_imagedata);
    printf("  Image Width: %u\n", header->image_width);
    printf("  Image Height: %u\n", header->image_height);
    free(header);

    fclose(fp);
    hm_reader_free(reader);
    hm_parser_free(parser);

    return 0;
}
