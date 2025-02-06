#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

typedef struct {
    uint8_t marker;
    uint16_t length;
    uint8_t data[0]; 
} JpegSegment;

static HammerParser* parse_uint16(void);
static HammerParser* parse_jpeg_segment(void);

static HammerParser* parse_uint16(void) {
    return hammer_map(hammer_be_uint16(), hammer_value_new_uint16);
}

static HammerParser* parse_jpeg_segment(void) {
    return hammer_map(hammer_seq(
                hammer_expect_uint8(0xFF),
                hammer_uint8(),
                parse_uint16(),
                hammer_bytes_count(hammer_ref(parse_uint16))
            ),
            (HammerMapFunc) (HammerValue* v) {
                uint16_t len = hammer_value_get_uint16(v->value[2]);
                JpegSegment* segment = malloc(sizeof(JpegSegment) + len - 2);
                segment->marker = 0xFF;
                segment->marker = hammer_value_get_uint8(v->value[1]);
                segment->length = len;
                memcpy(segment->data, hammer_value_get_bytes(v->value[3]), len - 2);
                return hammer_value_new_pointer(segment);
            });
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <jpeg_file>\n", argv[0]);
        return 1;
    }

    FILE* fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t* buffer = (uint8_t*)malloc(fsize);
    fread(buffer, 1, fsize, fp);
    fclose(fp);

    HammerParser* parser = hammer_many(parse_jpeg_segment);
    HammerResult* result = hammer_parse(parser, buffer, fsize);

    if (result->success) {
        printf("JPEG parsing successful!\n");
        HammerValue* segments = result->value;
        for (size_t i = 0; i < hammer_value_get_array_len(segments); ++i) {
            JpegSegment* segment = (JpegSegment*)hammer_value_get_pointer(hammer_value_get_array_element(segments, i));
            printf("Segment Marker: 0x%02X, Length: %u\n", segment->marker, segment->length);
            free(segment);
        }
    } else {
        fprintf(stderr, "JPEG parsing failed at offset %zu: %s\n", result->offset, result->error);
    }

    hammer_result_free(result);
    hammer_parser_free(parser);
    free(buffer);

    return 0;
}
