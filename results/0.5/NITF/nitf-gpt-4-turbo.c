#include <hammer/hammer.h>
#include <hammer/glue.h>

// Basic NITF types
static HParser *nitf_length;
static HParser *nitf_date_time;
static HParser *nitf_header;
static HParser *nitf_image_subheader;
static HParser *nitf_image_data;
static HParser *nitf_image_segment;
static HParser *nitf_file;

static void init_nitf_parser() {
    // Length field, typically 6 characters long
    nitf_length = h_fixedint(6);

    // Date and time field, typically 14 characters long
    nitf_date_time = h_fixedint(14);

    // NITF Header
    nitf_header = h_sequence(
        h_ch_range('0', '9', 9), // File Profile Name
        nitf_length,
        nitf_date_time,
        NULL
    );

    // NITF Image Subheader
    nitf_image_subheader = h_sequence(
        h_ch_range('0', '9', 10), // Image Identifier 1
        nitf_length,
        nitf_date_time,
        NULL
    );

    // NITF Image Data
    nitf_image_data = h_bytes(1);

    // NITF Image Segment
    nitf_image_segment = h_sequence(
        nitf_image_subheader,
        nitf_image_data,
        NULL
    );

    // NITF File
    nitf_file = h_sequence(
        nitf_header,
        h_many(nitf_image_segment),
        NULL
    );
}

int main(int argc, char *argv[]) {
    init_nitf_parser();

    // Assuming input data is provided as a file
    if (argc > 1) {
        size_t len;
        uint8_t *input = h_slurp_file(argv[1], &len);
        HParseResult *result = h_parse(nitf_file, input, len);
        if (result) {
            printf("Parse successful!\n");
            h_pprint(stdout, result->ast, 0, 4);
            h_parse_result_free(result);
        } else {
            printf("Parse failed!\n");
        }
        free(input);
    } else {
        printf("No input file provided.\n");
    }

    return 0;
}