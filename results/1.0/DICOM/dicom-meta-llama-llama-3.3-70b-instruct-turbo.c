#include <stdint.h>
#include <stdbool.h>
#include <string.h>

typedef struct {
    uint16_t group;
    uint16_t element;
    uint32_t vr;
    uint32_t length;
    uint8_t* value;
} dicom_tag_t;

typedef struct {
    dicom_tag_t tag;
    uint8_t* data;
} dicom_item_t;

typedef struct {
    uint16_t transfer_syntax_uid[64];
} dicom_prefix_t;

typedef struct {
    dicom_prefix_t prefix;
    dicom_item_t items[1024];
    uint16_t item_count;
} dicom_frame_t;

void dicom_parse_frame(dicom_frame_t* frame, uint8_t* data, uint32_t length) {
    // Parse prefix
    memcpy(frame->prefix.transfer_syntax_uid, data, 64);
    data += 64;
    length -= 64;

    // Parse items
    frame->item_count = 0;
    while (length > 0) {
        // Read tag
        frame->items[frame->item_count].tag.group = *(uint16_t*)data;
        data += 2;
        length -= 2;
        frame->items[frame->item_count].tag.element = *(uint16_t*)data;
        data += 2;
        length -= 2;

        // Read vr
        frame->items[frame->item_count].tag.vr = *(uint32_t*)data;
        data += 4;
        length -= 4;

        // Read length
        frame->items[frame->item_count].tag.length = *(uint32_t*)data;
        data += 4;
        length -= 4;

        // Read value
        frame->items[frame->item_count].value = data;
        data += frame->items[frame->item_count].tag.length;
        length -= frame->items[frame->item_count].tag.length;

        frame->item_count++;
    }
}

void dicom_write_frame(dicom_frame_t* frame, uint8_t* data) {
    // Write prefix
    memcpy(data, frame->prefix.transfer_syntax_uid, 64);
    data += 64;

    // Write items
    for (uint16_t i = 0; i < frame->item_count; i++) {
        // Write tag
        *(uint16_t*)data = frame->items[i].tag.group;
        data += 2;
        *(uint16_t*)data = frame->items[i].tag.element;
        data += 2;

        // Write vr
        *(uint32_t*)data = frame->items[i].tag.vr;
        data += 4;

        // Write length
        *(uint32_t*)data = frame->items[i].tag.length;
        data += 4;

        // Write value
        memcpy(data, frame->items[i].value, frame->items[i].tag.length);
        data += frame->items[i].tag.length;
    }
}

int main() {
    dicom_frame_t frame;
    uint8_t data[1024];
    uint32_t length = 1024;

    dicom_parse_frame(&frame, data, length);

    // Use frame data here

    dicom_write_frame(&frame, data);

    return 0;
}