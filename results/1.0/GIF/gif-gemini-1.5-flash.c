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
    //  Add more fields to represent nested structures as needed.  This is a placeholder.
} GIFHeader;

// Helper function to read a fixed-size string
static HParser* fixed_string(const char* str) {
    size_t len = strlen(str);
    return h_string(str, len);
}

// Helper function to read a uint16_t in little-endian format
static HParser* uint16_le() {
    return h_le_uint16();
}

// Helper function to read a uint8_t
static HParser* uint8() {
    return h_uint8();
}

// Helper function for packed fields (needs further implementation to extract flags)
static HParser* packed_fields() {
    return h_uint8();
}

//Helper function for variable length blocks
static HParser* variable_length_block(){
  return h_sequence(h_uint8(), h_many(h_uint8()));
}

// Function to map parsed data to GIFHeader struct (incomplete - needs full implementation)
static void* map_gif_header(void* data) {
    // This function needs to be fully implemented to correctly populate the GIFHeader struct from the parsed data.
    //  It's currently a placeholder.
    return data;
}

// GIF 89a Parser
HParser* gif_parser() {
    return h_sequence(
        fixed_string("GIF89a"),
        h_map(h_sequence(
                uint16_le(),
                uint16_le(),
                packed_fields(),
                uint8(),
                uint8(),
                h_many(h_sequence(
                    fixed_string(","),
                    uint16_le(),
                    uint16_le(),
                    uint16_le(),
                    uint16_le(),
                    packed_fields(),
                    h_optional(variable_length_block()),
                    variable_length_block()
                )),
                h_many(h_sequence(
                    fixed_string("!"),
                    variable_length_block()
                )),
                fixed_string(";")
            ), map_gif_header)
    );
}


int main(int argc, char** argv) {
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

    char* buffer = (char*)malloc(fileSize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, fileSize, file);
    fclose(file);

    hammer_result_t result = hammer_parse(gif_parser(), buffer, fileSize);

    if (result.success) {
        printf("GIF parsed successfully!\n");
        // Access parsed data via result.value (cast to GIFHeader*) - remember to fully implement map_gif_header
        GIFHeader* header = (GIFHeader*)result.value;
        printf("Width: %u, Height: %u\n", header->width, header->height); // Example access
    } else {
        fprintf(stderr, "GIF parsing failed at position %zu: %s\n", result.position, result.error);
    }

    free(buffer);
    return 0;
}

