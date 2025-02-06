#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// PNG Signature
HParser *png_signature_parser() {
    return h_sequence(h_bits(8, false), h_bits(8, false), h_bits(8, false), h_bits(8, false), h_bits(8, false), h_bits(8, false), h_bits(8, false), h_bits(8, false), NULL);
}

// Chunk Length
HParser *chunk_length_parser() {
    return h_uint32();
}

// Chunk Type
HParser *chunk_type_parser() {
    return h_bits(32, false);
}

// Chunk Data
HParser *chunk_data_parser(HParser *length_parser) {
    return h_length_value(length_parser, h_uint8());
}

// Chunk CRC
HParser *chunk_crc_parser() {
    return h_uint32();
}

// Chunk
HParser *chunk_parser() {
    HParser *length_parser = chunk_length_parser();
    return h_sequence(length_parser, chunk_type_parser(), chunk_data_parser(length_parser), chunk_crc_parser(), NULL);
}

// IHDR Chunk
HParser *ihdr_chunk_parser() {
    return h_sequence(h_uint32(), h_uint32(), h_uint8(), h_uint8(), h_uint8(), h_uint8(), h_uint8(), NULL);
}

// PLTE Chunk
HParser *plte_chunk_parser() {
    return h_repeat(h_sequence(h_uint8(), h_uint8(), h_uint8(), NULL), h_uint32());
}

// IDAT Chunk
HParser *idat_chunk_parser() {
    return h_length_value(chunk_length_parser(), h_uint8());
}

// IEND Chunk
HParser *iend_chunk_parser() {
    return h_sequence(chunk_length_parser(), chunk_type_parser(), chunk_crc_parser(), NULL);
}

// tRNS Chunk
HParser *trns_chunk_parser() {
    return h_length_value(chunk_length_parser(), h_uint8());
}

// gAMA Chunk
HParser *gama_chunk_parser() {
    return h_uint32();
}

// cHRM Chunk
HParser *chrm_chunk_parser() {
    return h_sequence(h_uint32(), h_uint32(), h_uint32(), h_uint32(), h_uint32(), h_uint32(), h_uint32(), h_uint32(), NULL);
}

// sRGB Chunk
HParser *srgb_chunk_parser() {
    return h_uint8();
}

// iCCP Chunk
HParser *iccp_chunk_parser() {
    return h_sequence(h_length_value(h_uint32(), h_uint8()), h_uint8(), h_length_value(h_uint32(), h_uint8()), NULL);
}

// tEXt Chunk
HParser *text_chunk_parser() {
    return h_sequence(h_length_value(h_uint32(), h_uint8()), h_length_value(h_uint32(), h_uint8()), NULL);
}

// zTXt Chunk
HParser *ztxt_chunk_parser() {
    return h_sequence(h_length_value(h_uint32(), h_uint8()), h_uint8(), h_length_value(h_uint32(), h_uint8()), NULL);
}

// iTXt Chunk
HParser *itxt_chunk_parser() {
    return h_sequence(h_length_value(h_uint32(), h_uint8()), h_uint8(), h_uint8(), h_length_value(h_uint32(), h_uint8()), h_length_value(h_uint32(), h_uint8()), h_length_value(h_uint32(), h_uint8()), NULL);
}

// bKGD Chunk
HParser *bkgd_chunk_parser() {
    return h_length_value(chunk_length_parser(), h_uint8());
}

// pHYs Chunk
HParser *phys_chunk_parser() {
    return h_sequence(h_uint32(), h_uint32(), h_uint8(), NULL);
}

// sBIT Chunk
HParser *sbit_chunk_parser() {
    return h_length_value(chunk_length_parser(), h_uint8());
}

// hIST Chunk
HParser *hist_chunk_parser() {
    return h_length_value(chunk_length_parser(), h_uint8());
}

// tIME Chunk
HParser *time_chunk_parser() {
    return h_sequence(h_uint16(), h_uint8(), h_uint8(), h_uint8(), h_uint8(), h_uint8(), NULL);
}

// PNG Parser
HParser *png_parser() {
    return h_sequence(png_signature_parser(), h_repeat(chunk_parser(), h_uint32()), NULL);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParseResult *result = h_parse(png_parser(), buffer, file_size);
    if (result) {
        printf("Parsing successful!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed!\n");
    }

    free(buffer);
    return 0;
}