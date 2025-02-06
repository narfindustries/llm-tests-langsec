#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Forward declarations
HParser* init_http_parser(void);
HParser* init_request_line(void);
HParser* init_status_line(void);
HParser* init_header_fields(void);
HParser* init_message_body(void);

// Helper parsers
static HParser* init_token(void) {
    return h_many1(h_not_in((const uint8_t*)"\0\r\n\t ()<>@,;:\\\"/[]?={}", 17));
}

static HParser* init_quoted_string(void) {
    HParser* escaped = h_sequence(h_ch('\\'), h_ch_range(0x00, 0xFF), NULL);
    HParser* unescaped = h_not_in((const uint8_t*)"\"\\", 2);
    HParser* content = h_choice(escaped, unescaped, NULL);
    return h_sequence(h_ch('"'), h_many(content), h_ch('"'), NULL);
}

static HParser* init_http_version(void) {
    return h_sequence(h_token((const uint8_t*)"HTTP/", 5),
                     h_ch_range('0', '9'),
                     h_ch('.'),
                     h_ch_range('0', '9'),
                     NULL);
}

static HParser* init_method(void) {
    return h_choice(h_token((const uint8_t*)"GET", 3),
                   h_token((const uint8_t*)"POST", 4),
                   h_token((const uint8_t*)"PUT", 3),
                   h_token((const uint8_t*)"DELETE", 6),
                   h_token((const uint8_t*)"HEAD", 4),
                   h_token((const uint8_t*)"OPTIONS", 7),
                   h_token((const uint8_t*)"TRACE", 5),
                   h_token((const uint8_t*)"CONNECT", 7),
                   NULL);
}

static HParser* init_request_uri(void) {
    return h_many1(h_not_in((const uint8_t*)" \r\n", 3));
}

static HParser* init_status_code(void) {
    return h_repeat_n(h_ch_range('0', '9'), 3);
}

static HParser* init_reason_phrase(void) {
    return h_sequence(h_many(h_not_in((const uint8_t*)"\r\n", 2)),
                     h_token((const uint8_t*)"\r\n", 2),
                     NULL);
}

static HParser* init_field_name(void) {
    return init_token();
}

static HParser* init_field_value(void) {
    HParser* space = h_ch(' ');
    HParser* content = h_choice(init_token(), init_quoted_string(), space, NULL);
    return h_sequence(h_many(content),
                     h_token((const uint8_t*)"\r\n", 2),
                     NULL);
}

static HParser* init_header_field(void) {
    return h_sequence(init_field_name(),
                     h_ch(':'),
                     h_optional(h_ch(' ')),
                     init_field_value(),
                     NULL);
}

HParser* init_request_line(void) {
    return h_sequence(init_method(),
                     h_ch(' '),
                     init_request_uri(),
                     h_ch(' '),
                     init_http_version(),
                     h_token((const uint8_t*)"\r\n", 2),
                     NULL);
}

HParser* init_status_line(void) {
    return h_sequence(init_http_version(),
                     h_ch(' '),
                     init_status_code(),
                     h_ch(' '),
                     init_reason_phrase(),
                     NULL);
}

HParser* init_header_fields(void) {
    return h_many(init_header_field());
}

HParser* init_message_body(void) {
    return h_many(h_ch_range(0x00, 0xFF));
}

HParser* init_http_parser(void) {
    return h_sequence(h_choice(init_request_line(),
                             init_status_line(),
                             NULL),
                     init_header_fields(),
                     h_token((const uint8_t*)"\r\n", 2),
                     h_optional(init_message_body()),
                     NULL);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open input file");
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
        perror("Failed to read input file");
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
    return result ? 0 : 1;
}