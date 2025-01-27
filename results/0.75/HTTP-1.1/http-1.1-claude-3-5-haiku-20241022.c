#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <string.h>

static HParser* http_version;
static HParser* method;
static HParser* request_uri;
static HParser* header_name;
static HParser* header_value;
static HParser* header;
static HParser* headers;
static HParser* request_line;
static HParser* http_request;

static const char* METHOD_NAMES[] = {
    "GET", "POST", "PUT", "DELETE", "HEAD", "OPTIONS", "TRACE", "CONNECT", "PATCH"
};

static HParser* create_method_parser() {
    HParsedToken* method_token = NULL;
    HParser* method_parser = h_choice(
        h_string("GET"),
        h_string("POST"),
        h_string("PUT"),
        h_string("DELETE"),
        h_string("HEAD"),
        h_string("OPTIONS"),
        h_string("TRACE"),
        h_string("CONNECT"),
        h_string("PATCH"),
        NULL
    );
    return method_parser;
}

static HParser* create_http_version_parser() {
    return h_choice(
        h_string("HTTP/1.0"),
        h_string("HTTP/1.1"),
        h_string("HTTP/2.0"),
        NULL
    );
}

static HParser* create_request_uri_parser() {
    return h_many1(h_not_char(' '));
}

static HParser* create_header_name_parser() {
    return h_many1(h_not_char(':'));
}

static HParser* create_header_value_parser() {
    return h_many1(h_not_char('\r'));
}

static HParser* create_headers_parser() {
    HParser* header_parser = h_sequence(
        h_name_mem("name", header_name),
        h_char(':'),
        h_whitespace(),
        h_name_mem("value", header_value),
        h_char('\r'),
        h_char('\n'),
        NULL
    );
    return h_many(header_parser);
}

static HParser* create_request_line_parser() {
    return h_sequence(
        h_name_mem("method", method),
        h_whitespace(),
        h_name_mem("uri", request_uri),
        h_whitespace(),
        h_name_mem("version", http_version),
        h_char('\r'),
        h_char('\n'),
        NULL
    );
}

static HParser* create_http_request_parser() {
    return h_sequence(
        h_name_mem("request_line", request_line),
        h_name_mem("headers", headers),
        h_char('\r'),
        h_char('\n'),
        NULL
    );
}

void init_parsers() {
    method = create_method_parser();
    http_version = create_http_version_parser();
    request_uri = create_request_uri_parser();
    header_name = create_header_name_parser();
    header_value = create_header_value_parser();
    headers = create_headers_parser();
    request_line = create_request_line_parser();
    http_request = create_http_request_parser();
}

void cleanup_parsers() {
    h_destroy_parser(method);
    h_destroy_parser(http_version);
    h_destroy_parser(request_uri);
    h_destroy_parser(header_name);
    h_destroy_parser(header_value);
    h_destroy_parser(headers);
    h_destroy_parser(request_line);
    h_destroy_parser(http_request);
}

int main() {
    init_parsers();

    const char* test_request = 
        "GET /index.html HTTP/1.1\r\n"
        "Host: www.example.com\r\n"
        "User-Agent: Mozilla/5.0\r\n"
        "\r\n";

    HParseResult* result = h_parse(http_request, (const uint8_t*)test_request, strlen(test_request));

    if (result && result->ast) {
        printf("Successfully parsed HTTP request\n");
    } else {
        printf("Failed to parse HTTP request\n");
    }

    h_parse_result_free(result);
    cleanup_parsers();
    return 0;
}