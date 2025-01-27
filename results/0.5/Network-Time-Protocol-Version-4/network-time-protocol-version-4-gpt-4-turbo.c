#include <hammer/hammer.h>
#include <hammer/glue.h>

// Define basic types for NTP
static HParser *ntp_timestamp;
static HParser *ntp_header;
static HParser *ntp_message;

// Helper function to create timestamp parser
static void init_timestamp() {
    ntp_timestamp = h_sequence(h_bits(32, false), h_bits(32, false), NULL);
}

// Helper function to create NTP header parser
static void init_header() {
    ntp_header = h_sequence(
        h_bits(2, false),  // Leap Indicator
        h_bits(3, false),  // Version
        h_bits(3, false),  // Mode
        h_bits(8, false),  // Stratum
        h_bits(8, false),  // Poll Interval
        h_bits(8, false),  // Precision
        h_bits(32, false), // Root Delay
        h_bits(32, false), // Root Dispersion
        h_bits(32, false), // Reference Identifier
        ntp_timestamp,     // Reference Timestamp
        ntp_timestamp,     // Originate Timestamp
        ntp_timestamp,     // Receive Timestamp
        ntp_timestamp,     // Transmit Timestamp
        NULL
    );
}

// Helper function to create NTP message parser
static void init_message() {
    ntp_message = h_sequence(
        ntp_header,
        h_end_p(),  // Ensure the parser consumes all input
        NULL
    );
}

// Main function to initialize parsers
int main(int argc, char **argv) {
    HParser *parser;

    // Initialize component parsers
    init_timestamp();
    init_header();
    init_message();

    // Build the top-level parser
    parser = ntp_message;

    // Compile the parser with Hammer
    if (!h_compile(parser, H_COMPILE_ALL)) {
        fprintf(stderr, "Failed to compile parser\n");
        return EXIT_FAILURE;
    }

    // Use the parser...

    return EXIT_SUCCESS;
}