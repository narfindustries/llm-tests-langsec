#include <hammer/hammer.h>
#include <hammer/glue.h>

#define MAX_DIMENSIONS 5
#define MAX_QUALITY 100

typedef struct {
    int width;
    int height;
    int channels;
    int quality;
    uint8_t* pixel_data;
} JPEGImage;

static HParser* jpeg_image_parser() {
    // Define width, height, channels, and quality parsers
    HParser* width_parser = h_int32();
    HParser* height_parser = h_int32();
    HParser* channels_parser = h_int_range(1, 4);
    HParser* quality_parser = h_int_range(1, MAX_QUALITY);

    // Define pixel data parser (placeholder)
    HParser* pixel_data_parser = h_many(h_uint8());

    // Combine parsers into a single struct parser
    return h_sequence(
        width_parser,
        height_parser,
        channels_parser,
        quality_parser,
        pixel_data_parser,
        NULL
    );
}

static HParseResult* jpeg_semantic_action(const HParseResult* p, void* user_data) {
    JPEGImage* image = malloc(sizeof(JPEGImage));
    
    // Extract parsed values safely
    image->width = *(int*)h_seq_index(p, 0);
    image->height = *(int*)h_seq_index(p, 1);
    image->channels = *(int*)h_seq_index(p, 2);
    image->quality = *(int*)h_seq_index(p, 3);
    
    // Handle pixel data
    HArray* pixel_array = (HArray*)h_seq_index(p, 4);
    image->pixel_data = malloc(pixel_array->used);
    memcpy(image->pixel_data, pixel_array->elements, pixel_array->used);

    return h_make_result(NULL, image);
}

HParser* jpeg_parser() {
    HParser* parser = jpeg_image_parser();
    return h_semantic_action(parser, jpeg_semantic_action, NULL, NULL);
}

void jpeg_image_free(JPEGImage* image) {
    if (image) {
        free(image->pixel_data);
        free(image);
    }
}

int main() {
    HParser* parser = jpeg_parser();
    return 0;
}