#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef struct {
    uint8_t marker;
    uint16_t length;
    uint8_t* data;
} JpegMarker;

typedef struct {
    //  Add your JPEG data structures here as needed for a full implementation.
    // This is a placeholder; a real JPEG parser would need significantly more.
} JpegData;

HammerParser marker_parser() {
    return hammer_seq(hammer_uint8(0xFF), hammer_uint8(), (HammerMapFunc) [](HammerValue v1, HammerValue v2){
        uint8_t marker = *(uint8_t*)v2.ptr;
        return (HammerValue){.ptr = &marker, .type = HAMMER_VALUE_UINT8};
    });
}

HammerParser length_parser() {
    return hammer_uint16_be();
}

HammerParser data_parser(uint16_t len) {
    return hammer_bytes(len);
}

HammerParser jpeg_marker_parser() {
    return hammer_seq(marker_parser(), length_parser(), (HammerParser)data_parser, (HammerValue){.ptr: &(uint16_t){0}, .type: HAMMER_VALUE_UINT16}, (HammerMapFunc) [](HammerValue marker, HammerValue len, HammerValue data){
        JpegMarker* m = malloc(sizeof(JpegMarker));
        if (m == NULL) {
            perror("malloc failed");
            exit(1);
        }
        m->marker = *(uint8_t*)marker.ptr;
        m->length = *(uint16_t*)len.ptr;
        m->data = malloc(*(uint16_t*)len.ptr);
        if (m->data == NULL) {
            perror("malloc failed");
            free(m);
            exit(1);
        }
        memcpy(m->data, data.ptr, *(uint16_t*)len.ptr);
        return (HammerValue){.ptr = m, .type = HAMMER_VALUE_PTR};
    });
}

HammerParser jpeg_parser() {
    return hammer_many(jpeg_marker_parser());
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <jpeg_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fileSize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *buffer = (uint8_t *)malloc(fileSize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(fp);
        return 1;
    }

    fread(buffer, 1, fileSize, fp);
    fclose(fp);

    HammerResult result = hammer_parse(jpeg_parser(), buffer, fileSize);

    if (result.success) {
        printf("JPEG file parsed successfully!\n");
        JpegMarker* markers = (JpegMarker*)result.value.ptr;
        for(size_t i = 0; i < result.value.len; i++){
            printf("Marker: 0x%02X, Length: %u\n", markers[i].marker, markers[i].length);
            free(markers[i].data);
            free(markers + i);
        }
        free(markers);
    } else {
        fprintf(stderr, "JPEG parsing failed at offset %zu: %s\n", result.offset, result.error);
    }

    free(buffer);
    return 0;
}
