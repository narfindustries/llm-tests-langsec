#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef struct {
    uint16_t width;
    uint16_t height;
    uint8_t packed_fields;
    uint8_t background_color_index;
    uint8_t pixel_aspect_ratio;
} GIFLogicalScreenDescriptor;

typedef struct {
    uint16_t image_left_position;
    uint16_t image_top_position;
    uint16_t image_width;
    uint16_t image_height;
    uint8_t packed_fields;
} GIFImageDescriptor;


// Helper function to read a specific number of bytes
static HResult read_bytes(HParser p, void *buf, size_t len) {
    return h_consume_n(p, len, buf);
}

// Define parsers for GIF elements
static HParser gif_signature = h_string("GIF89a");

static HParser gif_logical_screen_descriptor = h_map(
    h_struct(sizeof(GIFLogicalScreenDescriptor),
             h_uint16_field(offsetof(GIFLogicalScreenDescriptor, width)),
             h_uint16_field(offsetof(GIFLogicalScreenDescriptor, height)),
             h_uint8_field(offsetof(GIFLogicalScreenDescriptor, packed_fields)),
             h_uint8_field(offsetof(GIFLogicalScreenDescriptor, background_color_index)),
             h_uint8_field(offsetof(GIFLogicalScreenDescriptor, pixel_aspect_ratio))),
    h_field("logical_screen_descriptor")
);

static HParser gif_global_color_table = h_many(h_bytes(3));

static HParser gif_image_descriptor = h_map(
    h_struct(sizeof(GIFImageDescriptor),
             h_uint16_field(offsetof(GIFImageDescriptor, image_left_position)),
             h_uint16_field(offsetof(GIFImageDescriptor, image_top_position)),
             h_uint16_field(offsetof(GIFImageDescriptor, image_width)),
             h_uint16_field(offsetof(GIFImageDescriptor, image_height)),
             h_uint8_field(offsetof(GIFImageDescriptor, packed_fields))),
    h_field("image_descriptor")
);

static HParser gif_local_color_table = h_many(h_bytes(3));

static HParser gif_image_data = h_many(h_bytes(1)); //Simplified - actual LZW decoding needed

static HParser gif_graphics_control_extension = h_sequence(
    h_bytes(1), //Extension introducer
    h_bytes(1), //Extension label
    h_bytes(4), //Data
    h_bytes(1)  //Block terminator
);

static HParser gif_comment_extension = h_sequence(
    h_bytes(1), //Extension introducer
    h_bytes(1), //Extension label
    h_many(h_bytes(1)), //Data sub-blocks
    h_bytes(1)  //Block terminator
);

static HParser gif_plain_text_extension = h_sequence( //Simplified
    h_bytes(1), //Extension introducer
    h_bytes(1), //Extension label
    h_many(h_bytes(1)), //Data sub-blocks
    h_bytes(1)  //Block terminator
);

static HParser gif_application_extension = h_sequence( //Simplified
    h_bytes(1), //Extension introducer
    h_bytes(1), //Extension label
    h_many(h_bytes(1)), //Data sub-blocks
    h_bytes(1)  //Block terminator
);


static HParser gif_extension_block = h_choice(
    &gif_graphics_control_extension,
    &gif_comment_extension,
    &gif_plain_text_extension,
    &gif_application_extension
);

static HParser gif_image_block = h_sequence(
    h_byte(0x2C),
    &gif_image_descriptor,
    h_optional(h_byte(1), &gif_local_color_table),
    &gif_image_data
);

static HParser gif_file = h_sequence(
    &gif_signature,
    &gif_logical_screen_descriptor,
    h_optional(h_byte(1), &gif_global_color_table),
    h_many(&gif_extension_block),
    h_many(&gif_image_block),
    h_byte(0x3B)
);


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

    HParser parser = h_bind(h_bytes(fsize), buffer, &gif_file);
    HResult result = h_parse(parser, buffer, fsize);

    if (h_result_is_success(result)) {
        printf("GIF file parsed successfully!\n");
        // Access parsed data using h_get_field() etc.  (Requires further implementation)
    } else {
        fprintf(stderr, "GIF parsing failed: %s\n", h_result_error(result));
    }

    free(buffer);
    return 0;
}
