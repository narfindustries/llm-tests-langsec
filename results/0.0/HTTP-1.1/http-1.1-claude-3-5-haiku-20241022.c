#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <string.h>

// HTTP Method Definitions
static const HParser* http_method;

// HTTP Version Definitions
static const HParser* http_version;

// Header Field Definitions
static const HParser* header_name;
static const HParser* header_value;
static const HParser* header_field;

// Request Line Definitions
static const HParser* request_line;

// Headers Definitions
static const HParser* headers;

// Message Body Definitions
static const HParser* message_body;

// Complete HTTP Request Parser
static const HParser* http_request;

static HParsedToken* action_print_method(const HParseResult* p, void* user_data) {
    return h_make_str(p->ast);
}

static HParsedToken* action_print_version(const HParseResult* p, void* user_data) {
    return h_make_str(p->ast);
}

void init_http_parsers() {
    // HTTP Method Parser
    http_method = h_choice(
        h_string("GET"),
        h_string("POST"),
        h_string("PUT"),
        h_string("DELETE"),
        h_string("HEAD"),
        h_string("OPTIONS"),
        NULL
    );

    // HTTP Version Parser
    http_version = h_choice(
        h_string("HTTP/1.0"),
        h_string("HTTP/1.1"),
        h_string("HTTP/2.0"),
        NULL
    );

    // Header Name Parser
    header_name = h_many1(h_not_char(':'));

    // Header Value Parser
    header_value = h_many1(h_not_char('\r'));

    // Header Field Parser
    header_field = h_sequence(
        header_name,
        h_char(':'),
        h_whitespace(),
        header_value,
        h_string("\r\n"),
        NULL
    );

    // Request Line Parser
    request_line = h_sequence(
        http_method,
        h_whitespace(),
        h_many1(h_not_char(' ')),  // Request URI
        h_whitespace(),
        http_version,
        h_string("\r\n"),
        NULL
    );

    // Headers Parser
    headers = h_many(header_field);

    // Message Body Parser (optional)
    message_body = h_optional(h_many(h_any_char()));

    // Complete HTTP Request Parser
    http_request = h_sequence(
        request_line,
        headers,
        h_string("\r\n"),
        message_body,
        NULL
    );
}

int main() {
    init_http_parsers();
    return 0;
}