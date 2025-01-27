#include <hammer/hammer.h>
#include <hammer/glue.h>

// Define HTTP methods as an enumeration
static HParser *http_method;
static void init_http_method() {
    H_RULE(DELETE, h_token("DELETE"));
    H_RULE(GET, h_token("GET"));
    H_RULE(HEAD, h_token("HEAD"));
    H_RULE(POST, h_token("POST"));
    H_RULE(PUT, h_token("PUT"));
    H_RULE(CONNECT, h_token("CONNECT"));
    H_RULE(OPTIONS, h_token("OPTIONS"));
    H_RULE(TRACE, h_token("TRACE"));
    H_RULE(PATCH, h_token("PATCH"));

    http_method = h_choice(DELETE, GET, HEAD, POST, PUT, CONNECT, OPTIONS, TRACE, PATCH, NULL);
}

// Define HTTP version
static HParser *http_version;
static void init_http_version() {
    http_version = h_sequence(h_token("HTTP/"), h_ch_range('1', '1'), h_ch('.'), h_ch_range('0', '1'), NULL);
}

// Define HTTP status code
static HParser *status_code;
static void init_status_code() {
    status_code = h_int_range(h_uint(), 100, 599);
}

// Define request line for HTTP request
static HParser *request_line;
static void init_request_line() {
    H_RULE(space, h_ch(' '));
    H_RULE(uri, h_plus(h_not_from(" \r\n"))); // Simplified URI
    H_RULE(crlf, h_token("\r\n"));

    request_line = h_sequence(http_method, space, uri, space, http_version, crlf, NULL);
}

// Define status line for HTTP response
static HParser *status_line;
static void init_status_line() {
    H_RULE(space, h_ch(' '));
    H_RULE(reason_phrase, h_plus(h_not_from("\r\n"))); // Simplified reason phrase
    H_RULE(crlf, h_token("\r\n"));

    status_line = h_sequence(http_version, space, status_code, space, reason_phrase, crlf, NULL);
}

// Define header field
static HParser *header_field;
static void init_header_field() {
    H_RULE(field_name, h_plus(h_ch_range('A', 'Z')));
    H_RULE(colon_space, h_token(": "));
    H_RULE(field_value, h_plus(h_not_from("\r\n")));
    H_RULE(crlf, h_token("\r\n"));

    header_field = h_sequence(field_name, colon_space, field_value, crlf, NULL);
}

// Define headers (0 or more headers)
static HParser *headers;
static void init_headers() {
    headers = h_many(header_field);
}

// Define message body (simplified)
static HParser *message_body;
static void init_message_body() {
    message_body = h_greedy(h_any(), 0);
}

// Define full HTTP request
static HParser *http_request;
static void init_http_request() {
    http_request = h_sequence(request_line, headers, message_body, NULL);
}

// Define full HTTP response
static HParser *http_response;
static void init_http_response() {
    http_response = h_sequence(status_line, headers, message_body, NULL);
}

int main(int argc, char **argv) {
    HParser *parser;

    init_http_method();
    init_http_version();
    init_status_code();
    init_request_line();
    init_status_line();
    init_header_field();
    init_headers();
    init_message_body();
    init_http_request();
    init_http_response();

    parser = h_choice(http_request, http_response, NULL);
    if (!parse_hammer(parser, argv[1])) {
        fprintf(stderr, "Parsing failed.\n");
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}