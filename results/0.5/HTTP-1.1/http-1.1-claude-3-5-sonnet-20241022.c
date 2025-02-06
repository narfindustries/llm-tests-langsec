#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Forward declarations
HParser* init_http_parser(void);
HParser* init_request_line(void);
HParser* init_header_field(void);

// Utility parsers
HParser* create_token(void) {
    return h_not_in((const uint8_t*)" \t\r\n", 4);
}

HParser* create_sp(void) {
    return h_ch(' ');
}

HParser* create_crlf(void) {
    return h_sequence(h_ch('\r'), h_ch('\n'), NULL);
}

HParser* create_digit(void) {
    return h_ch_range('0', '9');
}

HParser* create_alpha(void) {
    return h_choice(h_ch_range('a', 'z'), h_ch_range('A', 'Z'), NULL);
}

// Method parsers
HParser* create_method(void) {
    return h_choice(
        h_token((const uint8_t*)"GET", 3),
        h_token((const uint8_t*)"POST", 4),
        h_token((const uint8_t*)"HEAD", 4),
        h_token((const uint8_t*)"PUT", 3),
        h_token((const uint8_t*)"DELETE", 6),
        h_token((const uint8_t*)"CONNECT", 7),
        h_token((const uint8_t*)"OPTIONS", 7),
        h_token((const uint8_t*)"TRACE", 5),
        NULL
    );
}

// URI parser
HParser* create_uri(void) {
    return h_many1(h_not_in((const uint8_t*)" \t\r\n", 4));
}

// HTTP version parser
HParser* create_http_version(void) {
    return h_sequence(
        h_token((const uint8_t*)"HTTP/", 5),
        create_digit(),
        h_ch('.'),
        create_digit(),
        NULL
    );
}

// Request line parser
HParser* init_request_line(void) {
    return h_sequence(
        create_method(),
        create_sp(),
        create_uri(),
        create_sp(),
        create_http_version(),
        create_crlf(),
        NULL
    );
}

// Header value parser
HParser* create_header_value(void) {
    return h_many1(h_not_in((const uint8_t*)"\r\n", 2));
}

// Header parsers
HParser* create_cache_control(void) {
    return h_sequence(
        h_token((const uint8_t*)"Cache-Control: ", 14),
        h_choice(
            h_token((const uint8_t*)"no-cache", 8),
            h_token((const uint8_t*)"no-store", 8),
            h_token((const uint8_t*)"public", 6),
            h_token((const uint8_t*)"private", 7),
            NULL
        ),
        NULL
    );
}

HParser* create_connection(void) {
    return h_sequence(
        h_token((const uint8_t*)"Connection: ", 12),
        h_choice(
            h_token((const uint8_t*)"close", 5),
            h_token((const uint8_t*)"keep-alive", 10),
            NULL
        ),
        NULL
    );
}

HParser* create_content_type(void) {
    return h_sequence(
        h_token((const uint8_t*)"Content-Type: ", 13),
        create_header_value(),
        NULL
    );
}

HParser* create_content_length(void) {
    return h_sequence(
        h_token((const uint8_t*)"Content-Length: ", 15),
        h_many1(create_digit()),
        NULL
    );
}

// Header field parser
HParser* init_header_field(void) {
    return h_choice(
        create_cache_control(),
        create_connection(),
        create_content_type(),
        create_content_length(),
        NULL
    );
}

// Complete HTTP message parser
HParser* init_http_parser(void) {
    return h_sequence(
        init_request_line(),
        h_many(h_sequence(init_header_field(), create_crlf(), NULL)),
        create_crlf(),
        h_many(h_uint8()),
        NULL
    );
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