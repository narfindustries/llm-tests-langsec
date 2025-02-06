#include <hammer/hammer.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// Forward declarations
HParser* init_http_parser(void);
HParser* init_request_line(void);
HParser* init_status_line(void);
HParser* init_headers(void);
HParser* init_body(void);

// Helper parsers
static HParser* token(void) {
    return h_many1(h_ch_range(33, 126));
}

static HParser* sp(void) {
    return h_ch(' ');
}

static HParser* crlf(void) {
    return h_sequence(h_ch('\r'), h_ch('\n'), NULL);
}

static HParser* http_version(void) {
    return h_sequence(h_token((const uint8_t*)"HTTP/", 5), 
                     h_ch_range('0', '9'),
                     h_ch('.'),
                     h_ch_range('0', '9'),
                     NULL);
}

// Method parser
static HParser* method(void) {
    return h_choice(h_token((const uint8_t*)"OPTIONS", 7),
                   h_token((const uint8_t*)"GET", 3),
                   h_token((const uint8_t*)"HEAD", 4),
                   h_token((const uint8_t*)"POST", 4),
                   h_token((const uint8_t*)"PUT", 3),
                   h_token((const uint8_t*)"DELETE", 6),
                   h_token((const uint8_t*)"TRACE", 5),
                   h_token((const uint8_t*)"CONNECT", 7),
                   NULL);
}

// Request-URI parser
static HParser* request_uri(void) {
    return h_many1(h_choice(h_ch_range('a', 'z'),
                           h_ch_range('A', 'Z'),
                           h_ch_range('0', '9'),
                           h_ch('/'),
                           h_ch('?'),
                           h_ch('='),
                           h_ch('&'),
                           h_ch('-'),
                           h_ch('_'),
                           h_ch('.'),
                           h_ch('~'),
                           NULL));
}

// Status code parser
static HParser* status_code(void) {
    return h_repeat_n(h_ch_range('0', '9'), 3);
}

// Reason phrase parser
static HParser* reason_phrase(void) {
    return h_many(h_choice(h_ch_range(32, 126), sp(), NULL));
}

// Header field parsers
static HParser* field_name(void) {
    return h_many1(h_choice(h_ch_range('a', 'z'),
                           h_ch_range('A', 'Z'),
                           h_ch('-'),
                           NULL));
}

static HParser* field_value(void) {
    return h_many(h_choice(h_ch_range(32, 126), sp(), NULL));
}

static HParser* header_field(void) {
    return h_sequence(field_name(),
                     h_ch(':'),
                     h_many(sp()),
                     field_value(),
                     crlf(),
                     NULL);
}

// Request line parser implementation
HParser* init_request_line(void) {
    return h_sequence(method(),
                     sp(),
                     request_uri(),
                     sp(),
                     http_version(),
                     crlf(),
                     NULL);
}

// Status line parser implementation
HParser* init_status_line(void) {
    return h_sequence(http_version(),
                     sp(),
                     status_code(),
                     sp(),
                     reason_phrase(),
                     crlf(),
                     NULL);
}

// Headers parser implementation
HParser* init_headers(void) {
    return h_sequence(h_many(header_field()),
                     crlf(),
                     NULL);
}

// Body parser implementation
HParser* init_body(void) {
    return h_many(h_ch_range(0, 255));
}

// Main HTTP parser implementation
HParser* init_http_parser(void) {
    return h_sequence(h_choice(init_request_line(),
                             init_status_line(),
                             NULL),
                     init_headers(),
                     init_body(),
                     NULL);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t *input = malloc(size);
    if (!input) {
        perror("Failed to allocate memory");
        fclose(f);
        return 1;
    }

    if (fread(input, 1, size, f) != size) {
        perror("Failed to read file");
        free(input);
        fclose(f);
        return 1;
    }
    fclose(f);

    HParser *parser = init_http_parser();
    HParseResult *result = h_parse(parser, input, size);

    if (result) {
        printf("Parsing successful\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed\n");
    }

    free(input);
    return 0;
}