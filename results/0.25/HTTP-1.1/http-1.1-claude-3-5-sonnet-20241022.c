#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Forward declarations
HParser* init_http_parser(void);
HParser* init_request_line(void);
HParser* init_status_line(void);
HParser* init_headers(void);
HParser* init_body(void);

// Helper parsers
static HParser* init_token(void) {
    return h_many1(h_not_in((const uint8_t*)"()<>@,;:\\\"/[]?={} \t", 21));
}

static HParser* init_quoted_string(void) {
    HParser* escaped = h_sequence(h_ch('\\'), h_ch_range(0x00, 0xFF), NULL);
    HParser* regular = h_not_in((const uint8_t*)"\"\\", 2);
    HParser* content = h_many(h_choice(regular, escaped, NULL));
    return h_sequence(h_ch('"'), content, h_ch('"'), NULL);
}

static HParser* init_http_version(void) {
    return h_sequence(h_token((const uint8_t*)"HTTP/", 5),
                     h_ch_range('0', '9'),
                     h_ch('.'),
                     h_ch_range('0', '9'),
                     NULL);
}

// Method parser
static HParser* init_method(void) {
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

// Request-Line parser
HParser* init_request_line(void) {
    return h_sequence(init_method(),
                     h_ch(' '),
                     h_many1(h_not_in((const uint8_t*)" \t\r\n", 4)),
                     h_ch(' '),
                     init_http_version(),
                     h_token((const uint8_t*)"\r\n", 2),
                     NULL);
}

// Status-Line parser
HParser* init_status_line(void) {
    HParser* status_code = h_repeat_n(h_ch_range('0', '9'), 3);
    HParser* reason_phrase = h_many(h_ch_range(0x20, 0x7E));
    return h_sequence(init_http_version(),
                     h_ch(' '),
                     status_code,
                     h_ch(' '),
                     reason_phrase,
                     h_token((const uint8_t*)"\r\n", 2),
                     NULL);
}

// Header field parsers
static HParser* init_field_content(void) {
    HParser* continuation = h_sequence(h_token((const uint8_t*)"\r\n", 2),
                                     h_in((const uint8_t*)" \t", 2),
                                     NULL);
    return h_many1(h_choice(h_not_in((const uint8_t*)"\r\n", 2),
                           continuation,
                           NULL));
}

static HParser* init_header_field(void) {
    return h_sequence(init_token(),
                     h_ch(':'),
                     h_ch(' '),
                     init_field_content(),
                     h_token((const uint8_t*)"\r\n", 2),
                     NULL);
}

HParser* init_headers(void) {
    return h_many(init_header_field());
}

HParser* init_body(void) {
    return h_many(h_ch_range(0x00, 0xFF));
}

HParser* init_http_parser(void) {
    return h_sequence(h_choice(init_request_line(),
                             init_status_line(),
                             NULL),
                     init_headers(),
                     h_token((const uint8_t*)"\r\n", 2),
                     h_optional(init_body()),
                     NULL);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open input file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *input = malloc(size);
    if (!input) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    if (fread(input, 1, size, file) != size) {
        perror("Failed to read input file");
        free(input);
        fclose(file);
        return 1;
    }
    fclose(file);

    HParser *parser = init_http_parser();
    HParseResult *result = h_parse(parser, input, size);

    if (result) {
        printf("Parsing successful\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed\n");
    }

    free(input);
    return result ? 0 : 1;
}