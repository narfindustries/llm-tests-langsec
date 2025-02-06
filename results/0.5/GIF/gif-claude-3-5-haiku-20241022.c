#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <hammer/hammer.h>

typedef struct {
    char signature[3];
    char version[3];
    uint16_t logical_screen_width;
    uint16_t logical_screen_height;
    uint8_t packed_fields;
    uint8_t background_color_index;
    uint8_t pixel_aspect_ratio;
    
    struct {
        bool global_color_table_present;
        uint8_t color_resolution;
        bool sorted;
        uint8_t color_table_size;
    } screen_descriptor;
    
    struct {
        uint8_t* entries;
        size_t count;
    } global_color_table;
    
    struct {
        uint16_t left_position;
        uint16_t top_position;
        uint16_t width;
        uint16_t height;
        uint8_t packed_fields;
        
        struct {
            bool local_color_table_present;
            bool interlaced;
            bool sorted;
            uint8_t color_table_size;
        } image_descriptor;
        
        struct {
            uint8_t* entries;
            size_t count;
        } local_color_table;
        
        uint8_t lzw_min_code_size;
        uint8_t* compressed_data;
        size_t compressed_data_size;
    } image;
    
    struct {
        uint8_t* data;
        size_t size;
    } extensions[4];
    
    uint8_t trailer;
} GifFile;

HParsedToken* parse_gif_header(void* p) {
    HParser* signature = h_token("GIF", 3);
    HParser* version = h_choice(h_token("87a", 3), h_token("89a", 3), NULL);
    
    HParser* header = h_sequence(signature, version, NULL);
    return h_parse(header, p, NULL);
}

HParsedToken* parse_logical_screen_descriptor(void* p) {
    HParser* width = h_uint16();
    HParser* height = h_uint16();
    HParser* packed_fields = h_uint8();
    HParser* background_color = h_uint8();
    HParser* aspect_ratio = h_uint8();
    
    HParser* descriptor = h_sequence(width, height, packed_fields, 
                                     background_color, aspect_ratio, NULL);
    return h_parse(descriptor, p, NULL);
}

HParsedToken* parse_color_table(void* p, bool is_global) {
    HParser* color_entry = h_sequence(h_uint8(), h_uint8(), h_uint8(), NULL);
    HParser* color_table = h_repeat_n(color_entry, is_global ? 256 : 0);
    
    return h_parse(color_table, p, NULL);
}

HParsedToken* parse_image_descriptor(void* p) {
    HParser* separator = h_token(",", 1);
    HParser* left = h_uint16();
    HParser* top = h_uint16();
    HParser* width = h_uint16();
    HParser* height = h_uint16();
    HParser* packed_fields = h_uint8();
    
    HParser* descriptor = h_sequence(separator, left, top, width, height, packed_fields, NULL);
    return h_parse(descriptor, p, NULL);
}

HParsedToken* parse_extension_blocks(void* p) {
    HParser* extension_introducer = h_token("!", 1);
    HParser* extension_type = h_uint8();
    HParser* extension_data = h_many(h_uint8());
    
    HParser* extension = h_sequence(extension_introducer, extension_type, extension_data, NULL);
    return h_parse(extension, p, NULL);
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <gif_file>\n", argv[0]);
        return 1;
    }
    
    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }
    
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);
    
    uint8_t* buffer = malloc(file_size);
    fread(buffer, 1, file_size, file);
    fclose(file);
    
    HParser* gif_parser = h_sequence(
        parse_gif_header,
        parse_logical_screen_descriptor,
        parse_color_table,
        parse_image_descriptor,
        parse_color_table,
        parse_extension_blocks,
        h_token(";", 1),
        NULL
    );
    
    HParsedToken* result = h_parse(gif_parser, buffer, file_size);
    
    if (result) {
        printf("GIF parsed successfully\n");
        h_parse_result_free(result);
    } else {
        printf("GIF parsing failed\n");
    }
    
    free(buffer);
    return 0;
}