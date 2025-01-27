#include <hammer/hammer.h>

typedef struct {
    uint32_t file_header;
    uint16_t file_type;
    uint32_t file_length;
    char classification[1];
    char origination_station[4];
    char date_time[14];
    char title[80];
    char security_info[2];
} nitf_header_t;

HParser *nitf_header_parser(void) {
    return h_sequence(
        h_uint32(), // file_header
        h_uint16(), // file_type
        h_uint32(), // file_length
        h_repeat(h_uint8(), 1), // classification
        h_repeat(h_uint8(), 4), // origination_station
        h_repeat(h_uint8(), 14), // date_time
        h_repeat(h_uint8(), 80), // title
        h_repeat(h_uint8(), 2), // security_info
        NULL
    );
}

HParser *nitf_file_parser(void) {
    return h_sequence(
        nitf_header_parser(),
        // Add other segments or fields here as needed
        NULL
    );
}

int main(void) {
    HParser *parser = nitf_file_parser();
    // Add parsing logic here
    h_parser_free(parser);
    return 0;
}