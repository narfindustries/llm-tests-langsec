#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <string.h>

// HTTP Request Method Parsing
static HParser* http_method;

// HTTP Version Parsing 
static HParser* http_version;

// HTTP Header Parsing
static HParser* http_header;

// HTTP Request Line Parsing
static HParser* http_request_line;

// Full HTTP Request Parsing
static HParser* http_request;

// Method Definitions
static HParser* method_parser() {
    return h_choice(
        h_string_cs("GET"),
        h_string_cs("POST"),
        h_string_cs("PUT"),
        h_string_cs("DELETE"),
        h_string_cs("HEAD"),
        h_string_cs("OPTIONS"),
        h_string_cs("PATCH"),
        NULL
    );
}

// Version Definitions
static HParser* version_parser() {
    return h_sequence(
        h_string_cs("HTTP/"),
        h_choice(
            h_string_cs("1.0"),
            h_string_cs("1.1"),
            h_string_cs("2.0"),
            NULL
        ),
        NULL
    );
}

// URI/Path Parser
static HParser* uri_parser() {
    return h_many1(
        h_not_char(' ')
    );
}

// Header Parser 
static HParser* header_parser() {
    return h_sequence(
        h_many1(h_not_char(':')),  // Header Name
        h_char(':'),                // Separator
        h_many1(h_not_char('\r')),  // Header Value
        h_string_cs("\r\n"),        // Line Termination
        NULL
    );
}

// Request Line Parser
static HParser* request_line_parser() {
    return h_sequence(
        method_parser(),     // HTTP Method
        h_whitespace(),      // Whitespace
        uri_parser(),        // Request URI/Path
        h_whitespace(),      // Whitespace
        version_parser(),    // HTTP Version
        h_string_cs("\r\n"), // Line Termination
        NULL
    );
}

// Full Request Parser
static HParser* request_parser() {
    return h_sequence(
        request_line_parser(),       // Request Line
        h_many(header_parser()),     // Headers
        h_string_cs("\r\n"),         // Headers Termination
        NULL
    );
}

int main() {
    // Initialize Hammer Parser
    h_init();

    // Create Parsers
    http_method = method_parser();
    http_version = version_parser();
    http_header = header_parser();
    http_request_line = request_line_parser();
    http_request = request_parser();

    return 0;
}