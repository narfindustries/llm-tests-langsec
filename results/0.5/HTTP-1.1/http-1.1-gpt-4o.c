#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Define basic parsers for HTTP components
HParser *create_token_parser() {
    return h_many1(h_choice(h_ch_range('a', 'z'), h_ch_range('A', 'Z'), h_ch_range('0', '9'), h_ch('-'), h_ch('_'), NULL));
}

HParser *create_quoted_string_parser() {
    return h_seq(h_ch('"'), h_many(h_choice(h_ch_range(32, 33), h_ch_range(35, 126), NULL)), h_ch('"'), NULL);
}

HParser *create_http_version_parser() {
    return h_sequence(h_token("HTTP/", 5), h_many1(h_digit_p()), h_ch('.'), h_many1(h_digit_p()), NULL);
}

HParser *create_status_code_parser() {
    return h_repeat_n(h_digit_p(), 3);
}

HParser *create_reason_phrase_parser() {
    return h_many(h_choice(h_ch_range(32, 126), NULL));
}

// Define parsers for headers
HParser *create_general_header_parser() {
    return h_many(h_sequence(create_token_parser(), h_ch(':'), h_many(h_choice(h_ch_range(32, 126), NULL)), h_ch('\r'), h_ch('\n'), NULL));
}

HParser *create_request_header_parser() {
    return h_many(h_sequence(create_token_parser(), h_ch(':'), h_many(h_choice(h_ch_range(32, 126), NULL)), h_ch('\r'), h_ch('\n'), NULL));
}

HParser *create_response_header_parser() {
    return h_many(h_sequence(create_token_parser(), h_ch(':'), h_many(h_choice(h_ch_range(32, 126), NULL)), h_ch('\r'), h_ch('\n'), NULL));
}

HParser *create_entity_header_parser() {
    return h_many(h_sequence(create_token_parser(), h_ch(':'), h_many(h_choice(h_ch_range(32, 126), NULL)), h_ch('\r'), h_ch('\n'), NULL));
}

// Define parsers for request and response
HParser *create_request_parser() {
    return h_sequence(
        create_token_parser(),      // Method
        h_ch(' '),
        h_many(h_choice(h_ch_range(32, 126), NULL)), // Request-URI
        h_ch(' '),
        create_http_version_parser(),
        h_ch('\r'), h_ch('\n'),
        create_general_header_parser(),
        create_request_header_parser(),
        create_entity_header_parser(),
        h_ch('\r'), h_ch('\n'),
        h_many(h_any_byte_p()), // Message body
        NULL
    );
}

HParser *create_response_parser() {
    return h_sequence(
        create_http_version_parser(),
        h_ch(' '),
        create_status_code_parser(),
        h_ch(' '),
        create_reason_phrase_parser(),
        h_ch('\r'), h_ch('\n'),
        create_general_header_parser(),
        create_response_header_parser(),
        create_entity_header_parser(),
        h_ch('\r'), h_ch('\n'),
        h_many(h_any_byte_p()), // Message body
        NULL
    );
}

// Main function to read and parse HTTP message from a file
int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input file>\n", argv[0]);
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

    HParser *request_parser = create_request_parser();
    HParser *response_parser = create_response_parser();
    HParseResult *result = h_parse(request_parser, data, file_size);

    if (result) {
        printf("Parsed as HTTP request.\n");
        h_parse_result_free(result);
    } else {
        result = h_parse(response_parser, data, file_size);
        if (result) {
            printf("Parsed as HTTP response.\n");
            h_parse_result_free(result);
        } else {
            printf("Failed to parse as HTTP request or response.\n");
        }
    }

    h_parser_free(request_parser);
    h_parser_free(response_parser);
    free(data);

    return EXIT_SUCCESS;
}