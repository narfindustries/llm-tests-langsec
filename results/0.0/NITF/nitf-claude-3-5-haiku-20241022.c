#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <string.h>

// Define parser for NITF file structure
static HParser* nitf_file_parser() {
    // Define basic NITF header fields
    HParser* header_magic = h_literal_string("NITF");
    HParser* version = h_choice(
        h_literal_string("02.10"),
        h_literal_string("02.00"),
        NULL
    );
    
    // Define complex nested structures
    HParser* header_length = h_int_range(0, 99999);
    HParser* file_header = h_sequence(
        header_magic,
        version, 
        header_length,
        NULL
    );

    // Define data segments
    HParser* data_segment = h_choice(
        h_int_range(0, 0xFFFFFFFF),
        h_string_cs(),
        NULL
    );

    // Combine into complete file parser
    HParser* nitf_parser = h_sequence(
        file_header,
        h_many(data_segment),
        NULL
    );

    return nitf_parser;
}

int main() {
    // Initialize Hammer parser
    HParser* parser = nitf_file_parser();
    
    // Example input for testing
    const char* test_input = "NITF02.1012345";
    
    // Parse input
    HParseResult* result = h_parse(parser, 
        (const uint8_t*)test_input, 
        strlen(test_input)
    );

    // Validate and process result
    if (result && result->ast) {
        printf("NITF file parsed successfully\n");
        return 0;
    } else {
        printf("NITF file parsing failed\n");
        return 1;
    }
}