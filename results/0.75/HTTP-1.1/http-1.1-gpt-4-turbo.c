#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

static HParser *http_version;
static HParser *method;
static HParser *request_uri;
static HParser *status_code;
static HParser *reason_phrase;
static HParser *header;
static HParser *headers;
static HParser *message_body;

void init_parsers() {
    http_version = h_token("HTTP/1.1", 8);
    method = h_choice(
        h_sequence(h_string("OPTIONS", 7), NULL),
        h_sequence(h_string("GET", 3), NULL),
        h_sequence(h_string("HEAD", 4), NULL),
        h_sequence(h_string("POST", 4), NULL),
        h_sequence(h_string("PUT", 3), NULL),
        h_sequence(h_string("DELETE", 6), NULL),
        h_sequence(h_string("TRACE", 5), NULL),
        h_sequence(h_string("CONNECT", 7), NULL),
        NULL
    );
    request_uri = h_token(h_ch_range(0x21, 0x7E), 1);
    status_code = h_int_range(100, 599);
    reason_phrase = h_token(h_ch_range(0x20, 0x7E), 1);
    header = h_sequence(
        h_token(h_many1(h_ch_range(0x21, 0x7E))),
        h_ch(':'),
        h_whitespace(),
        h_token(h_many1(h_ch_range(0x21, 0x7E))),
        h_token("\r\n", 2),
        NULL
    );
    headers = h_many(header);
    message_body = h_token(h_many(h_any()));
}

HParser *http_request() {
    return h_sequence(
        method, h_ch(' '), request_uri, h_ch(' '), http_version, h_token("\r\n", 2),
        headers,
        message_body,
        NULL
    );
}

HParser *http_response() {
    return h_sequence(
        http_version, h_ch(' '), status_code, h_ch(' '), reason_phrase, h_token("\r\n", 2),
        headers,
        message_body,
        NULL
    );
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <http_message_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long length = ftell(file);
    fseek(file, 0, SEEK_SET);
    unsigned char *data = malloc(length);
    if (data) {
        fread(data, 1, length, file);
    }
    fclose(file);

    init_parsers();
    HParser *parser = http_request();  // Change to http_response() as needed

    HParseResult *result = h_parse(parser, data, length);
    if (result) {
        printf("HTTP message parsed successfully.\n");
    } else {
        printf("Failed to parse HTTP message.\n");
    }

    h_parse_result_free(result);
    free(data);

    return EXIT_SUCCESS;
}