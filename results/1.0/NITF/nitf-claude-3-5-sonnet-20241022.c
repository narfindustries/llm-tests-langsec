#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// NITF Parser definitions
HParser* nitf_fhdr;
HParser* nitf_fver;
HParser* nitf_clevel;
HParser* nitf_stype;
HParser* nitf_ostaid;
HParser* nitf_fdt;
HParser* nitf_ftitle;
HParser* nitf_fsclas;
HParser* nitf_header;
HParser* nitf_image_segment;
HParser* nitf_text_segment;
HParser* nitf_graphics_segment;
HParser* nitf_parser;

void init_parser(void) {
    // File Header parsers
    nitf_fhdr = h_token((const uint8_t*)"NITF", 4);
    nitf_fver = h_token((const uint8_t*)"02.10", 5);
    nitf_clevel = h_uint8();
    nitf_stype = h_token((const uint8_t*)"BF", 2);
    nitf_ostaid = h_length_value(h_int_range(h_uint8(), 1, 10), h_uint8());
    nitf_fdt = h_length_value(h_token((const uint8_t*)"CCYYMMDDhhmmss", 14), h_uint8());
    nitf_ftitle = h_length_value(h_int_range(h_uint8(), 1, 80), h_uint8());
    
    // Security Classification
    HParser* class_values[] = {
        h_ch('T'), h_ch('S'), h_ch('C'), h_ch('R'), h_ch('U')
    };
    nitf_fsclas = h_choice(class_values, 5);

    // Image Segment parsers
    HParser* im_token = h_token((const uint8_t*)"IM", 2);
    HParser* iid1 = h_length_value(h_int_range(h_uint8(), 1, 10), h_uint8());
    HParser* idatim = h_length_value(h_token((const uint8_t*)"CCYYMMDDhhmmss", 14), h_uint8());
    HParser* nrows = h_uint32();
    HParser* ncols = h_uint32();
    
    // Image representation types
    HParser* irep_values[] = {
        h_token((const uint8_t*)"MONO", 4),
        h_token((const uint8_t*)"RGB", 3),
        h_token((const uint8_t*)"RGB/LUT", 7)
    };
    HParser* irep = h_choice(irep_values, 3);

    // Build the complete image segment
    nitf_image_segment = h_sequence(im_token, iid1, idatim, nrows, ncols, irep, NULL);

    // Build the complete header
    nitf_header = h_sequence(nitf_fhdr, nitf_fver, nitf_clevel, nitf_stype,
                           nitf_ostaid, nitf_fdt, nitf_ftitle, nitf_fsclas, NULL);

    // Build the complete NITF parser
    nitf_parser = h_sequence(nitf_header, nitf_image_segment, NULL);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <nitf_file>\n", argv[0]);
        return 1;
    }

    FILE *f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open file");
        return 1;
    }

    // Get file size
    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    // Read file into buffer
    uint8_t *input = malloc(size);
    if (!input) {
        perror("Failed to allocate memory");
        fclose(f);
        return 1;
    }
    
    if (fread(input, 1, size, f) != size) {
        perror("Failed to read file");
        free(input);
        fclose(f);
        return 1;
    }
    fclose(f);

    // Initialize parser
    init_parser();

    // Parse input
    HParseResult *result = h_parse(nitf_parser, input, size);
    
    if (!result) {
        fprintf(stderr, "Failed to parse NITF file\n");
        free(input);
        return 1;
    }

    // Process result here
    // For now, just indicate success
    printf("Successfully parsed NITF file\n");

    // Cleanup
    h_parse_result_free(result);
    free(input);
    return 0;
}