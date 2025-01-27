#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <string.h>

static HParser* nitf_header;
static HParser* nitf_file;

// Basic Elementary Field Parsers
static HParser* parse_numeric_field(int length) {
    return h_repeat_n(h_in_range('0', '9'), length);
}

// NITF File Structure Parsers
static HParser* nitf_file_header() {
    return h_sequence(
        h_string("NITF"),                  // File Type Identifier 
        parse_numeric_field(2),            // Version
        parse_numeric_field(10),           // File Header Length
        NULL
    );
}

// Main File Parser
static HParser* create_nitf_parser() {
    nitf_file = h_choice(
        nitf_file_header(),
        NULL
    );
    return nitf_file;
}

int main(int argc, char** argv) {
    HParserBackend backend = HParserBackend_Packrat;
    h_init(&backend);

    nitf_header = create_nitf_parser();
    
    return 0;
}