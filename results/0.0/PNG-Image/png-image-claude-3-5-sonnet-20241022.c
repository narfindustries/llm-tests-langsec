#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

static const uint8_t PNG_SIGNATURE[] = {0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A};

HParser* init_png_parser() {
    HParser *png_signature = h_token(PNG_SIGNATURE, sizeof(PNG_SIGNATURE));
    HParser *chunk_length = h_uint32();
    HParser *chunk_type = h_repeat_n(h_ch_range('A', 'Z'), 4);
    HParser *crc = h_uint32();

    // IHDR fields
    HParser *width = h_uint32();
    HParser *height = h_uint32();
    HParser *bit_depth = h_uint8();
    HParser *color_type = h_uint8();
    HParser *compression_method = h_uint8();
    HParser *filter_method = h_uint8();
    HParser *interlace_method = h_uint8();

    // PLTE chunk
    HParser *rgb_triplet = h_sequence(h_uint8(), h_uint8(), h_uint8(), NULL);
    HParser *plte_data = h_repeat_n(rgb_triplet, 256);

    // tRNS chunk
    HParser *trns_gray = h_uint16();
    HParser *trns_rgb = h_sequence(h_uint16(), h_uint16(), h_uint16(), NULL);
    HParser *trns_palette = h_many(h_uint8());

    // cHRM chunk
    HParser *chrm_data = h_sequence(
        h_uint32(), h_uint32(),
        h_uint32(), h_uint32(),
        h_uint32(), h_uint32(),
        h_uint32(), h_uint32(),
        NULL
    );

    // Other chunks
    HParser *gama_data = h_uint32();
    HParser *profile_name = h_many1(h_ch_range(32, 126));
    HParser *iccp_data = h_sequence(profile_name, h_uint8(), h_many1(h_uint8()), NULL);
    HParser *srgb_data = h_uint8();
    
    // Using h_repeat_n instead of h_many_m_n for keyword
    HParser *keyword_char = h_ch_range(32, 126);
    HParser *keyword = h_length_value(
        h_int_range(h_uint8(), 1, 79),
        keyword_char
    );
    
    HParser *text_data = h_sequence(keyword, h_many1(h_uint8()), NULL);
    HParser *ztxt_data = h_sequence(keyword, h_uint8(), h_many1(h_uint8()), NULL);
    HParser *itxt_data = h_sequence(
        keyword, h_uint8(), h_uint8(),
        h_many(h_uint8()), h_many(h_uint8()),
        h_many(h_uint8()), NULL
    );

    HParser *bkgd_gray = h_uint16();
    HParser *bkgd_rgb = h_sequence(h_uint16(), h_uint16(), h_uint16(), NULL);
    HParser *bkgd_palette = h_uint8();
    HParser *phys_data = h_sequence(h_uint32(), h_uint32(), h_uint8(), NULL);
    HParser *time_data = h_sequence(
        h_uint16(), h_uint8(), h_uint8(),
        h_uint8(), h_uint8(), h_uint8(),
        NULL
    );

    HParser *idat_data = h_many1(h_uint8());
    HParser *iend_data = h_nothing_p();

    // Chunk wrapper
    HParser* chunk_wrapper(HParser *data_parser) {
        return h_sequence(chunk_length, chunk_type, data_parser, crc, NULL);
    }

    // Build chunk parsers
    HParser *ihdr = chunk_wrapper(
        h_sequence(width, height, bit_depth, color_type,
                  compression_method, filter_method, interlace_method, NULL)
    );
    
    HParser *plte = chunk_wrapper(plte_data);
    HParser *idat = chunk_wrapper(idat_data);
    HParser *iend = chunk_wrapper(iend_data);
    
    // Optional chunks
    HParser *trns = chunk_wrapper(h_choice(trns_gray, trns_rgb, trns_palette, NULL));
    HParser *chrm = chunk_wrapper(chrm_data);
    HParser *gama = chunk_wrapper(gama_data);
    HParser *iccp = chunk_wrapper(iccp_data);
    HParser *srgb = chunk_wrapper(srgb_data);
    HParser *text = chunk_wrapper(text_data);
    HParser *ztxt = chunk_wrapper(ztxt_data);
    HParser *itxt = chunk_wrapper(itxt_data);
    HParser *bkgd = chunk_wrapper(h_choice(bkgd_gray, bkgd_rgb, bkgd_palette, NULL));
    HParser *phys = chunk_wrapper(phys_data);
    HParser *time = chunk_wrapper(time_data);

    HParser *optional_chunks = h_many(h_choice(trns, chrm, gama, iccp, srgb,
                                             text, ztxt, itxt, bkgd, phys, time, NULL));

    return h_sequence(
        png_signature,
        ihdr,
        h_optional(plte),
        optional_chunks,
        h_many1(idat),
        iend,
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <png_file>\n", argv[0]);
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

    if (fread(buffer, 1, file_size, file) != file_size) {
        perror("Failed to read file");
        free(buffer);
        fclose(file);
        return 1;
    }

    HParser *png_parser = init_png_parser();
    HParseResult *result = h_parse(png_parser, buffer, file_size);

    if (result) {
        printf("PNG file parsed successfully\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse PNG file\n");
    }

    free(buffer);
    fclose(file);
    return result ? 0 : 1;
}