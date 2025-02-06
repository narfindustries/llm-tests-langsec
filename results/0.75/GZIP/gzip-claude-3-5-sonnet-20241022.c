#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

static bool check_flag(const HParsedToken* p, void* user_data) {
    uint8_t mask = (uint8_t)(uintptr_t)user_data;
    uint8_t flags = (uint8_t)p->uint;
    return (flags & mask) != 0;
}

static HParser* build_gzip_parser(void) {
    // Header fields
    HParser* id1_p = h_ch(0x1F);
    HParser* id2_p = h_ch(0x8B);
    HParser* cm_p = h_uint8();
    HParser* flg_p = h_uint8();
    HParser* mtime_p = h_uint32();
    HParser* xfl_p = h_uint8();
    HParser* os_p = h_uint8();
    
    // Optional fields
    HParser* xlen_p = h_uint16();
    HParser* extra_field_p = h_length_value(xlen_p, h_uint8());
    HParser* filename_p = h_many_until(h_uint8(), h_ch(0x00));
    HParser* comment_p = h_many_until(h_uint8(), h_ch(0x00));
    HParser* hcrc16_p = h_uint16();

    // Flag checks
    HParser* fextra_p = h_and(h_bits(4, 0x04), extra_field_p);
    HParser* fname_p = h_and(h_bits(4, 0x08), filename_p);
    HParser* fcomment_p = h_and(h_bits(4, 0x10), comment_p);
    HParser* fhcrc_p = h_and(h_bits(4, 0x02), hcrc16_p);

    // Optional fields sequence
    HParser* optional_fields = h_sequence(
        h_optional(fextra_p),
        h_optional(fname_p),
        h_optional(fcomment_p),
        h_optional(fhcrc_p),
        NULL
    );

    // Compressed data and footer
    HParser* compressed_data_p = h_many1(h_uint8());
    HParser* crc32_p = h_uint32();
    HParser* isize_p = h_uint32();

    // Complete GZIP format
    return h_sequence(
        id1_p,
        id2_p,
        cm_p,
        flg_p,
        mtime_p,
        xfl_p,
        os_p,
        optional_fields,
        compressed_data_p,
        crc32_p,
        isize_p,
        NULL
    );
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <gzip_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t* input = malloc(size);
    if (!input) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    if (fread(input, 1, size, file) != size) {
        perror("Failed to read file");
        free(input);
        fclose(file);
        return 1;
    }

    HParser* gzip_parser = build_gzip_parser();
    HParseResult* result = h_parse(gzip_parser, input, size);

    if (!result) {
        fprintf(stderr, "Failed to parse GZIP file\n");
        free(input);
        fclose(file);
        return 1;
    }

    printf("Successfully parsed GZIP file\n");

    h_parse_result_free(result);
    free(input);
    fclose(file);
    return 0;
}