#include <hammer/hammer.h>

// Define HTTP version rule
HParser *http_version = h_sequence(
    h_ch('H'), h_ch('T'), h_ch('T'), h_ch('P'), h_ch('/'),
    h_digit(), h_ch('.'), h_digit(),
    NULL
);

// Define HTTP method rule
HParser *http_method = h_choice(
    h_ignore_case(h_token("GET", 3)),
    h_ignore_case(h_token("POST", 4)),
    h_ignore_case(h_token("PUT", 3)),
    h_ignore_case(h_token("DELETE", 6)),
    h_ignore_case(h_token("HEAD", 4)),
    h_ignore_case(h_token("OPTIONS", 7)),
    h_ignore_case(h_token("PATCH", 5)),
    NULL
);

// Define HTTP header field rule
HParser *http_header_field = h_sequence(
    h_many1(h_choice(h_alpha(), h_ch('-'))),
    h_ch(':'), h_while(h_not(h_ch('\n'))), h_ch('\n'),
    NULL
);

// Define HTTP headers section rule
HParser *http_headers = h_many(http_header_field);

// Define HTTP request line rule
HParser *http_request_line = h_sequence(
    http_method, h_ch(' '),
    h_while(h_not(h_ch(' '))), h_ch(' '),
    http_version, h_ch('\n'),
    NULL
);

// Define HTTP request parser
HParser *http_request = h_sequence(
    http_request_line,
    http_headers,
    h_ch('\n'), // End of headers section
    NULL
);

// Entry point for parsing
int main(int argc, char **argv) {
    // Example HTTP request to parse
    const char *data = "GET /index.html HTTP/1.1\nHost: example.com\n\n";
    HParseResult *result = h_parse(http_request, (const uint8_t *)data, strlen(data));
    
    if (result) {
        printf("HTTP request parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse HTTP request.\n");
    }

    h_parser_free(http_request);
    return 0;
}