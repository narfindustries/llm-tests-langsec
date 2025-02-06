#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_HEADER_SIZE 8192

// Forward declarations
HParser* init_http_parser(void);
HParser* init_request_line(void);
HParser* init_header(void);
HParser* init_status_line(void);

// HTTP Methods
static const uint8_t* HTTP_METHODS[] = {
    (const uint8_t*)"GET",
    (const uint8_t*)"POST",
    (const uint8_t*)"PUT",
    (const uint8_t*)"DELETE",
    (const uint8_t*)"HEAD",
    (const uint8_t*)"OPTIONS",
    (const uint8_t*)"TRACE",
    (const uint8_t*)"CONNECT"
};

// Common parsers
static HParser* whitespace;
static HParser* crlf;
static HParser* token;
static HParser* quoted_string;

void init_common_parsers(void) {
    whitespace = h_whitespace(h_ch(' '));
    crlf = h_token((const uint8_t*)"\r\n", 2);
    token = h_many1(h_not_in((const uint8_t*)" \t\r\n():/<>@,;:\\\"{}[]?=", 22));
    quoted_string = h_sequence(h_ch('"'),
                              h_many1(h_not_in((const uint8_t*)"\"\\\r\n", 4)),
                              h_ch('"'),
                              NULL);
}

// Method parser
static HParser* init_method(void) {
    HParser* method_parsers[8];
    for(int i = 0; i < 8; i++) {
        method_parsers[i] = h_token(HTTP_METHODS[i], strlen((char*)HTTP_METHODS[i]));
    }
    return h_choice(method_parsers[0], method_parsers[1], method_parsers[2],
                   method_parsers[3], method_parsers[4], method_parsers[5],
                   method_parsers[6], method_parsers[7], NULL);
}

// URI parser
static HParser* init_uri(void) {
    return h_many1(h_not_in((const uint8_t*)" \r\n", 3));
}

// HTTP version parser
static HParser* init_http_version(void) {
    HParser* digit = h_ch_range('0', '9');
    return h_sequence(h_token((const uint8_t*)"HTTP/", 5),
                     digit,
                     h_ch('.'),
                     digit,
                     NULL);
}

// Request line parser
HParser* init_request_line(void) {
    return h_sequence(init_method(),
                     whitespace,
                     init_uri(),
                     whitespace,
                     init_http_version(),
                     crlf,
                     NULL);
}

// Status line parser
HParser* init_status_line(void) {
    HParser* digit = h_ch_range('0', '9');
    HParser* status_code = h_repeat_n(digit, 3);
    return h_sequence(init_http_version(),
                     whitespace,
                     status_code,
                     whitespace,
                     h_many1(h_not_in((const uint8_t*)"\r\n", 2)),
                     crlf,
                     NULL);
}

// Header field parser
static HParser* init_header_field(void) {
    return h_sequence(token,
                     h_ch(':'),
                     h_optional(whitespace),
                     h_many1(h_choice(token, quoted_string, NULL)),
                     crlf,
                     NULL);
}

// Headers parser
HParser* init_header(void) {
    return h_many(init_header_field());
}

// Message body parser
static HParser* init_message_body(void) {
    return h_many(h_uint8());
}

// Complete HTTP message parser
HParser* init_http_parser(void) {
    return h_sequence(h_choice(init_request_line(), init_status_line(), NULL),
                     init_header(),
                     crlf,
                     h_optional(init_message_body()),
                     NULL);
}

int main(int argc, char** argv) {
    if(argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* fp = fopen(argv[1], "rb");
    if(!fp) {
        perror("Failed to open input file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    size_t size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t* input = malloc(size);
    if(fread(input, 1, size, fp) != size) {
        perror("Failed to read input file");
        fclose(fp);
        free(input);
        return 1;
    }
    fclose(fp);

    init_common_parsers();
    HParser* http_parser = init_http_parser();
    
    HParseResult* result = h_parse(http_parser, input, size);
    if(result) {
        printf("Successfully parsed HTTP message\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse HTTP message\n");
    }

    free(input);
    return 0;
}