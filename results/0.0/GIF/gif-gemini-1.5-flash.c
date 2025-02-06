#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

// Helper functions for easier parsing
static HParser* uint8() { return HParser_uint8(); }
static HParser* uint16() { return HParser_uint16_be(); } 
static HParser* string(size_t len) { return HParser_string(len); }
static HParser* bytes(size_t len) { return HParser_bytes(len); }

// GIF Parser
typedef struct {
    char signature[6];
    uint16_t width;
    uint16_t height;
    uint8_t packedFields;
    uint8_t backgroundColorIndex;
    uint8_t pixelAspectRatio;
    uint8_t globalColorTable[768]; 
    HParser* imageDescriptors; // Array of image descriptors
    uint8_t trailer;
} GIF;


typedef struct {
    uint8_t imageSeparator;
    uint16_t imageLeftPosition;
    uint16_t imageTopPosition;
    uint16_t imageWidth;
    uint16_t imageHeight;
    uint8_t imagePackedFields;
    uint8_t localColorTable[768]; 
    uint8_t* imageData;
    size_t imageDataSize;
} ImageDescriptor;


static HParser* imageDescriptorParser() {
    return HParser_seq(
        HParser_uint8(),
        uint16(),
        uint16(),
        uint16(),
        uint16(),
        uint8(),
        HParser_maybe(HParser_bytes(768)),
        HParser_collect(HParser_many1(uint8()))
    );
}


static HParser* gifParser() {
  HParser* imageDescriptorArrayParser = HParser_collect(HParser_many1(imageDescriptorParser()));
    HParser* parser = HParser_seq(
        string(6), 
        uint16(), 
        uint16(), 
        uint8(), 
        uint8(), 
        uint8(), 
        HParser_maybe(HParser_bytes(768)), 
        imageDescriptorArrayParser, 
        uint8() 
    );
    return parser;
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <gif_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long fileSize = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t* buffer = (uint8_t*)malloc(fileSize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, fileSize, file);
    fclose(file);

    HParser* parser = gifParser();
    HResult result = HParser_parse(parser, buffer, fileSize);

    if (result.success) {
        GIF* gif = (GIF*)result.value;
        printf("GIF parsed successfully!\n");

        // Access and print GIF data here...
        HArray* imageDescriptors = (HArray*)gif->imageDescriptors;
        for (size_t i = 0; i < imageDescriptors->len; i++) {
          ImageDescriptor* desc = (ImageDescriptor*)imageDescriptors->data[i];
          free(desc->imageData);
          free(desc);
        }
        HArray_free(imageDescriptors);
        free(gif);
    } else {
        fprintf(stderr, "GIF parsing failed: %s\n", result.error);
    }

    free(buffer);
    HParser_free(parser);
    return 0;
}
