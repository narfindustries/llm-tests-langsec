#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define parsers for HTTP tokens
static HParser *token;
static HParser *quoted_string;
static HParser *LWS;
static HParser *OWS;
static HParser *header_value;

// Initialize parsers for basic tokens
void init_parsers() {
    token = h_token((const uint8_t *)"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!#$%&'*+-.^_`|~", 1);
    quoted_string = h_sequence(h_ch('"'), h_many(h_ch_range(0x20, 0x7E)), h_ch('"'), NULL);
    LWS = h_optional(h_ch(' '));
    OWS = h_optional(h_ch(' '));
    header_value = h_choice(token, quoted_string, NULL);
}

// Define parsers for HTTP headers
HParser *header() {
    return h_sequence(token, h_ch(':'), OWS, header_value, LWS, NULL);
}

HParser *headers() {
    return h_many(header());
}

// Define parsers for HTTP request line
HParser *request_line() {
    HParser *method = token;
    HParser *request_uri = token; // Simplified URI
    HParser *http_version = h_sequence(h_token((const uint8_t *)"HTTP/", 5), h_ch('1'), h_ch('.'), h_choice(h_ch('0'), h_ch('1'), NULL), NULL);
    return h_sequence(method, h_ch(' '), request_uri, h_ch(' '), http_version, h_ch('\r'), h_ch('\n'), NULL);
}

// Define parsers for HTTP message
HParser *http_message() {
    return h_sequence(request_line(), headers(), h_ch('\r'), h_ch('\n'), NULL);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <http_request_file>\n", argv[0]);
        return 1;
    }

    // Initialize parsers
    init_parsers();

    // Read input file
    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("File opening failed");
        return EXIT_FAILURE;
    }

    fseek(fp, 0, SEEK_END);
    size_t length = ftell(fp);
    fseek(fp, 0, SEEK_SET);
    uint8_t *data = malloc(length);
    if (data) {
        fread(data, 1, length, fp);
    }
    fclose(fp);

    // Parse HTTP message
    HParseResult *result = h_parse(http_message(), data, length);
    if (result) {
        printf("Parse successful.\n");
    } else {
        printf("Parse failed.\n");
    }

    free(data);
    return 0;
}