#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <hammer/hammer.h>
#include <hammer/parsers.h>

typedef struct {
    HBytes *fhdr;
    int64_t clevel;
    HBytes *stype;
    HBytes *ostaid;
    HBytes *fdt;
    HBytes *ftitle;
    HBytes *fscop;
    int64_t fscpys;
    HBytes *encryp;
} NITFFileHeader;

typedef struct {
    HBytes *itype;
    HBytes *irep;
    HBytes *icat;
    HBytes *icords;
    HBytes *igeolo;
} NITFImageSubheader;

typedef struct {
    NITFFileHeader *file_header;
    NITFImageSubheader *image_subheader;
} NITFFile;

HParsedToken* parse_file_header(void* context) {
    HParser *fhdr = h_token((const uint8_t*)"NITF02.10", 9);
    HParser *clevel = h_int_range(h_ch_range('0', '9'), 0, 9);
    HParser *stype = h_choice(h_token((const uint8_t*)"C", 1), h_token((const uint8_t*)"R", 1), NULL);
    HParser *ostaid = h_repeat_n(h_ch_range('A', 'Z'), 10);
    HParser *fdt = h_repeat_n(h_ch_range('0', '9'), 14);
    HParser *ftitle = h_repeat_n(h_ch_range('A', 'Z'), 80);
    HParser *fscop = h_choice(
        h_token((const uint8_t*)"TOP SECRET", 10),
        h_token((const uint8_t*)"SECRET", 6),
        h_token((const uint8_t*)"CONFIDENTIAL", 12),
        NULL
    );
    HParser *fscpys = h_int_range(h_ch_range('0', '9'), 0, 999999);
    HParser *encryp = h_choice(h_token((const uint8_t*)"0", 1), h_token((const uint8_t*)"1", 1), NULL);

    HParser *file_header = h_sequence(
        fhdr, clevel, stype, ostaid, fdt, 
        ftitle, fscop, fscpys, encryp, NULL
    );

    HParseResult *result = h_parse(file_header, context, 0);
    return result ? (HParsedToken*)result->ast : NULL;
}

HParsedToken* parse_image_subheader(void* context) {
    HParser *itype = h_choice(
        h_token((const uint8_t*)"MONO", 4), 
        h_token((const uint8_t*)"RGB", 3), 
        h_token((const uint8_t*)"MULTI", 5), 
        NULL
    );
    HParser *irep = h_choice(
        h_token((const uint8_t*)"MONO", 4), 
        h_token((const uint8_t*)"RGB", 3), 
        h_token((const uint8_t*)"MULTI", 5), 
        NULL
    );
    HParser *icat = h_choice(
        h_token((const uint8_t*)"VIS", 3), 
        h_token((const uint8_t*)"IR", 2), 
        h_token((const uint8_t*)"SAR", 3), 
        NULL
    );
    HParser *icords = h_choice(
        h_token((const uint8_t*)"N", 1), 
        h_token((const uint8_t*)"S", 1), 
        h_token((const uint8_t*)"D", 1), 
        NULL
    );
    HParser *igeolo = h_repeat_n(h_ch_range('0', '9'), 60);

    HParser *image_subheader = h_sequence(
        itype, irep, icat, icords, igeolo, NULL
    );

    HParseResult *result = h_parse(image_subheader, context, 0);
    return result ? (HParsedToken*)result->ast : NULL;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <nitf_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }

    size_t read_size = fread(buffer, 1, file_size, file);
    if (read_size != file_size) {
        perror("File read error");
        free(buffer);
        fclose(file);
        return 1;
    }

    HParsedToken *file_header_result = parse_file_header(buffer);
    HParsedToken *image_subheader_result = parse_image_subheader(buffer);

    if (file_header_result && image_subheader_result) {
        printf("NITF file parsed successfully\n");
    } else {
        printf("NITF file parsing failed\n");
    }

    free(buffer);
    fclose(file);
    return 0;
}