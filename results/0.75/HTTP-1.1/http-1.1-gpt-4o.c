#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

HParser *create_http_parser();

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *parser = create_http_parser();
    HParseResult *result = h_parse(parser, data, file_size);

    if (result) {
        printf("Parsing successful!\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Parsing failed!\n");
    }

    free(data);
    h_parser_unref(parser);

    return EXIT_SUCCESS;
}

HParser *create_http_parser() {
    // Basic parsers
    HParser *space = h_ch(' ');
    HParser *token = h_many1(h_choice(space, h_ch('\t'), NULL));
    HParser *alpha = h_choice(h_ch_range('a', 'z'), h_ch_range('A', 'Z'), NULL);
    HParser *digit = h_ch_range('0', '9');
    HParser *crlf = h_sequence(h_ch('\r'), h_ch('\n'), NULL);

    // Header field parsers
    HParser *field_name = h_many1(h_choice(alpha, digit, h_ch('-'), h_ch('_'), NULL));
    HParser *field_value = h_many(h_not(crlf));
    HParser *header_field = h_sequence(
        field_name,
        h_sequence(token, h_ch(':'), token, field_value, crlf, NULL),
        NULL
    );

    // Start-line parsers
    HParser *http_version = h_sequence(
        h_token((const uint8_t *)"HTTP/", 5), digit, h_ch('.'), digit, NULL
    );
    HParser *request_method = h_many1(alpha);
    HParser *request_target = h_many(h_not(h_choice(space, h_ch('\t'), h_ch('\r'), h_ch('\n'), NULL)));
    HParser *request_line = h_sequence(
        request_method,
        token,
        request_target,
        token,
        http_version,
        crlf,
        NULL
    );

    // Full HTTP message parser
    HParser *http_message = h_sequence(
        request_line,
        h_many(header_field),
        crlf,  // End of headers
        h_optional(h_many(h_ch_range(0, 255))),  // Optional body
        NULL
    );

    return http_message;
}