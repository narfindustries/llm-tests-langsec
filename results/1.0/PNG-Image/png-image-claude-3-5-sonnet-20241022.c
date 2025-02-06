#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// PNG Parser definitions
HParser* png_signature;
HParser* chunk_length;
HParser* chunk_type;
HParser* chunk_data;
HParser* chunk_crc;
HParser* chunk;
HParser* png_file;

// IHDR fields
HParser* ihdr_width;
HParser* ihdr_height;
HParser* ihdr_bit_depth;
HParser* ihdr_color_type;
HParser* ihdr_compression;
HParser* ihdr_filter;
HParser* ihdr_interlace;
HParser* ihdr_chunk;

// PLTE fields
HParser* plte_entry;
HParser* plte_chunk;

// Ancillary chunks
HParser* trns_chunk;
HParser* gama_chunk;
HParser* chrm_chunk;
HParser* srgb_chunk;
HParser* iccp_chunk;
HParser* text_chunk;
HParser* ztxt_chunk;
HParser* itxt_chunk;
HParser* bkgd_chunk;
HParser* phys_chunk;
HParser* sbit_chunk;
HParser* time_chunk;
HParser* hist_chunk;
HParser* splt_chunk;

void init_parser() {
    // PNG Signature
    uint8_t signature[] = {0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A};
    png_signature = h_token(signature, 8);
    
    // Basic chunk structure
    chunk_length = h_uint32();
    chunk_type = h_repeat_n(h_ch_range('A', 'Z'), 4);
    chunk_data = h_length_value(h_uint32(), h_uint8());
    chunk_crc = h_uint32();
    
    // IHDR chunk
    ihdr_width = h_uint32();
    ihdr_height = h_uint32();
    ihdr_bit_depth = h_uint8();
    ihdr_color_type = h_uint8();
    ihdr_compression = h_uint8();
    ihdr_filter = h_uint8();
    ihdr_interlace = h_uint8();
    
    ihdr_chunk = h_sequence(
        h_token((uint8_t*)"IHDR", 4),
        ihdr_width,
        ihdr_height,
        ihdr_bit_depth,
        ihdr_color_type,
        ihdr_compression,
        ihdr_filter,
        ihdr_interlace,
        NULL
    );
    
    // PLTE chunk
    plte_entry = h_repeat_n(h_uint8(), 3);
    plte_chunk = h_sequence(
        h_token((uint8_t*)"PLTE", 4),
        h_many1(plte_entry),
        NULL
    );
    
    // Initialize other chunks (simplified for brevity)
    trns_chunk = h_sequence(
        h_token((uint8_t*)"tRNS", 4),
        h_many1(h_uint8()),
        NULL
    );
    
    gama_chunk = h_sequence(
        h_token((uint8_t*)"gAMA", 4),
        h_uint32(),
        NULL
    );
    
    // Combine all chunks
    chunk = h_choice(
        ihdr_chunk,
        plte_chunk,
        trns_chunk,
        gama_chunk,
        h_token((uint8_t*)"IEND", 4),
        NULL
    );
    
    // Complete PNG file
    png_file = h_sequence(
        png_signature,
        h_many1(chunk),
        NULL
    );
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <png_file>\n", argv[0]);
        return 1;
    }
    
    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }
    
    // Get file size
    fseek(file, 0, SEEK_END);
    size_t size = ftell(file);
    fseek(file, 0, SEEK_SET);
    
    // Read file
    uint8_t* data = malloc(size);
    if (fread(data, 1, size, file) != size) {
        perror("Failed to read file");
        fclose(file);
        free(data);
        return 1;
    }
    
    init_parser();
    
    HParseResult* result = h_parse(png_file, data, size);
    if (!result) {
        fprintf(stderr, "Failed to parse PNG file\n");
        fclose(file);
        free(data);
        return 1;
    }
    
    printf("Successfully parsed PNG file\n");
    
    h_parse_result_free(result);
    fclose(file);
    free(data);
    return 0;
}