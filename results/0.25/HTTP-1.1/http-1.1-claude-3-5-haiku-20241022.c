#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <string.h>

static HParser *http_request;
static HParser *http_method;
static HParser *http_uri;
static HParser *http_version;
static HParser *http_header;
static HParser *http_headers;
static HParser *http_body;

static const char* METHOD_STRINGS[] = {
    "GET", "POST", "PUT", "DELETE", "HEAD", "OPTIONS", "PATCH"
};

static HParser* make_http_parser() {
    http_method = h_choice_str(METHOD_STRINGS, sizeof(METHOD_STRINGS)/sizeof(const char*));
    http_uri = h_token_seq((uint8_t*)"/", 1);
    http_version = h_choice_str((const char*[]){"HTTP/1.0", "HTTP/1.1"}, 2);

    http_header = h_sequence(
        h_token_seq((uint8_t*)": ", 2),
        h_many(h_not_char('\r'))
    );

    http_headers = h_many(http_header);

    http_body = h_many(h_not_char('\0'));

    http_request = h_sequence(
        http_method,
        h_whitespace(),
        http_uri,
        h_whitespace(),
        http_version,
        h_token_seq((uint8_t*)"\r\n", 2),
        http_headers,
        h_token_seq((uint8_t*)"\r\n", 2),
        http_body
    );

    return http_request;
}

int main(int argc, char **argv) {
    make_http_parser();
    return 0;
}