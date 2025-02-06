#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Forward declarations
HParser* init_png_parser(void);
HParser* init_chunk_parser(void);
HParser* init_ihdr_parser(void);
HParser* init_plte_parser(void);
HParser* init_idat_parser(void);
HParser* init_iend_parser(void);
HParser* init_trns_parser(void);
HParser* init_gama_parser(void);
HParser* init_chrm_parser(void);
HParser* init_srgb_parser(void);
HParser* init_iccp_parser(void);
HParser* init_text_parser(void);
HParser* init_ztxt_parser(void);
HParser* init_itxt_parser(void);
HParser* init_bkgd_parser(void);
HParser* init_phys_parser(void);
HParser* init_sbit_parser(void);
HParser* init_splt_parser(void);
HParser* init_hist_parser(void);
HParser* init_time_parser(void);

HParser* init_png_parser(void) {
    uint8_t signature[] = {0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A};
    HParser* png_signature = h_token(signature, 8);
    HParser* chunks = h_many1(init_chunk_parser());
    return h_sequence(png_signature, chunks, NULL);
}

HParser* init_chunk_parser(void) {
    HParser* length = h_uint32();
    HParser* chunk_type = h_bits(32, false);
    HParser* chunk_data = h_length_value(length, h_uint8());
    HParser* crc = h_uint32();
    
    return h_sequence(length, chunk_type, chunk_data, crc, NULL);
}

HParser* init_ihdr_parser(void) {
    return h_sequence(
        h_uint32(), // width
        h_uint32(), // height
        h_uint8(),  // bit depth
        h_uint8(),  // color type
        h_uint8(),  // compression method
        h_uint8(),  // filter method
        h_uint8(),  // interlace method
        NULL
    );
}

HParser* init_plte_parser(void) {
    HParser* rgb_entry = h_sequence(h_uint8(), h_uint8(), h_uint8(), NULL);
    return h_many1(rgb_entry);
}

HParser* init_idat_parser(void) {
    return h_many1(h_uint8());
}

HParser* init_iend_parser(void) {
    return h_nothing_p();
}

HParser* init_trns_parser(void) {
    return h_many1(h_uint8());
}

HParser* init_gama_parser(void) {
    return h_uint32();
}

HParser* init_chrm_parser(void) {
    return h_sequence(
        h_uint32(), h_uint32(), // white point
        h_uint32(), h_uint32(), // red
        h_uint32(), h_uint32(), // green
        h_uint32(), h_uint32(), // blue
        NULL
    );
}

HParser* init_srgb_parser(void) {
    return h_uint8();
}

HParser* init_iccp_parser(void) {
    HParser* name = h_many(h_uint8());
    return h_sequence(
        name,
        h_ch(0x00),         // null terminator
        h_uint8(),          // compression method
        h_many1(h_uint8()), // compressed profile
        NULL
    );
}

HParser* init_text_parser(void) {
    HParser* keyword = h_many(h_uint8());
    return h_sequence(
        keyword,
        h_ch(0x00),      // null terminator
        h_many1(h_uint8()), // text
        NULL
    );
}

HParser* init_ztxt_parser(void) {
    HParser* keyword = h_many(h_uint8());
    return h_sequence(
        keyword,
        h_ch(0x00),         // null terminator
        h_uint8(),          // compression method
        h_many1(h_uint8()), // compressed text
        NULL
    );
}

HParser* init_itxt_parser(void) {
    HParser* keyword = h_many(h_uint8());
    HParser* lang_tag = h_many(h_uint8());
    HParser* trans_keyword = h_many(h_uint8());
    return h_sequence(
        keyword,
        h_ch(0x00),         // null terminator
        h_uint8(),          // compression flag
        h_uint8(),          // compression method
        lang_tag,
        h_ch(0x00),         // null terminator
        trans_keyword,
        h_ch(0x00),         // null terminator
        h_many1(h_uint8()), // text
        NULL
    );
}

HParser* init_bkgd_parser(void) {
    return h_many1(h_uint8());
}

HParser* init_phys_parser(void) {
    return h_sequence(
        h_uint32(), // x axis
        h_uint32(), // y axis
        h_uint8(),  // unit specifier
        NULL
    );
}

HParser* init_sbit_parser(void) {
    return h_many1(h_uint8());
}

HParser* init_splt_parser(void) {
    HParser* name = h_many(h_uint8());
    return h_sequence(
        name,
        h_ch(0x00),         // null terminator
        h_uint8(),          // sample depth
        h_many1(h_uint8()), // palette entries
        NULL
    );
}

HParser* init_hist_parser(void) {
    return h_many1(h_uint16());
}

HParser* init_time_parser(void) {
    return h_sequence(
        h_uint16(), // year
        h_uint8(),  // month
        h_uint8(),  // day
        h_uint8(),  // hour
        h_uint8(),  // minute
        h_uint8(),  // second
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <png_file>\n", argv[0]);
        return 1;
    }

    FILE *f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t *buffer = malloc(size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(f);
        return 1;
    }

    if (fread(buffer, 1, size, f) != size) {
        perror("Failed to read file");
        free(buffer);
        fclose(f);
        return 1;
    }

    HParser *png_parser = init_png_parser();
    HParseResult *result = h_parse(png_parser, buffer, size);

    if (result) {
        printf("Successfully parsed PNG file\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse PNG file\n");
    }

    free(buffer);
    fclose(f);
    return 0;
}