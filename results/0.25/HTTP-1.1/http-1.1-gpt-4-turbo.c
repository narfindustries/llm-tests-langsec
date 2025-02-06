#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define parsers for HTTP tokens based on RFC 2616
static HParser *http_version;
static HParser *method;
static HParser *uri;
static HParser *status_code;
static HParser *reason_phrase;
static HParser *header_name;
static HParser *header_value;
static HParser *request_line;
static HParser *status_line;
static HParser *header;
static HParser *message_body;
static HParser *http_request;
static HParser *http_response;

void init_parsers() {
    http_version = h_sequence(h_bytes("HTTP/", 5), h_ch_range('1', '1'), h_ch('.'), h_ch_range('0', '1'), NULL);
    method = h_choice(h_bytes("GET", 3), h_bytes("POST", 4), h_bytes("HEAD", 4), h_bytes("PUT", 3), h_bytes("DELETE", 6), h_bytes("TRACE", 5), h_bytes("OPTIONS", 7), h_bytes("CONNECT", 7), NULL);
    uri = h_token(h_many1(h_not_char(' ')));
    status_code = h_int_range(h_uint(), 100, 599);
    reason_phrase = h_token(h_many1(h_not_char('\r')));
    header_name = h_token(h_many1(h_not_char(':')));
    header_value = h_token(h_many1(h_not_char('\r')));
    request_line = h_sequence(method, h_whitespace(), uri, h_whitespace(), http_version, h_end_p(), NULL);
    status_line = h_sequence(http_version, h_whitespace(), status_code, h_whitespace(), reason_phrase, h_end_p(), NULL);
    header = h_sequence(header_name, h_ch(':'), h_whitespace(), header_value, h_end_p(), NULL);
    message_body = h_greedy(h_any(), NULL);
    http_request = h_sequence(request_line, h_many(header), h_optional(message_body), NULL);
    http_response = h_sequence(status_line, h_many(header), h_optional(message_body), NULL);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <http_message_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t length = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = malloc(length);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    if (fread(data, 1, length, file) != length) {
        perror("Failed to read file");
        free(data);
        fclose(file);
        return 1;
    }

    fclose(file);

    init_parsers();

    HParseResult *result = h_parse(http_request, data, length);
    if (!result) {
        result = h_parse(http_response, data, length);
    }

    if (result) {
        printf("Parsing succeeded.\n");
    } else {
        printf("Parsing failed.\n");
    }

    free(data);
    h_parse_result_free(result);
    return 0;
}