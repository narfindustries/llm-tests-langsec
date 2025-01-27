#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    uint8_t marker;
    uint16_t length;
    uint8_t marker_type;
} JpegMarker;

typedef struct {
    uint8_t precision;
    uint16_t height;
    uint16_t width;
} StartOfFrame;

typedef struct {
    JpegMarker marker;
    union {
        StartOfFrame sof;
    } data;
} JPEG;

void parse_jpeg(const uint8_t* bytes, size_t len) {
    size_t pos = 0;
    while (pos < len) {
        JpegMarker marker;
        marker.marker = bytes[pos];
        pos++;
        if (marker.marker == 0xFF) {
            marker.length = ((uint16_t)bytes[pos + 1] << 8) | bytes[pos + 2];
            pos += 2;
            marker.marker_type = bytes[pos];
            pos++;
            if (marker.marker_type == 0xC0) {
                StartOfFrame sof;
                sof.precision = bytes[pos];
                pos++;
                sof.height = ((uint16_t)bytes[pos] << 8) | bytes[pos + 1];
                pos += 2;
                sof.width = ((uint16_t)bytes[pos] << 8) | bytes[pos + 1];
                pos += 2;
                printf("SOFn precision: %u\n", sof.precision);
                printf("SOFn height: %u\n", sof.height);
                printf("SOFn width: %u\n", sof.width);
            } else {
                pos += marker.length - 2; // -2 because we already read 2 bytes for length
            }
        } else {
            break;
        }
    }
}

int main() {
    const uint8_t example_jpeg[] = {
        0xFF, 0xD8, 0xFF, 0xE0, 0x00, 0x10, 0x4A, 0x46, 0x49, 0x46, 0x00, 0x01,
        0x01, 0x00, 0x00, 0x48, 0x00, 0x48, 0x00, 0x00, 0xFF, 0xC0, 0x00, 0x11,
        0x08, 0x00, 0x30, 0x00, 0x30, 0x00, 0x00, 0x00, 0xFF, 0xDA, 0x00, 0x0C,
        0x03, 0x01, 0x00, 0x02, 0x11, 0x03, 0x11, 0x00, 0x3F, 0x00
    };
    parse_jpeg(example_jpeg, sizeof(example_jpeg));
    return 0;
}