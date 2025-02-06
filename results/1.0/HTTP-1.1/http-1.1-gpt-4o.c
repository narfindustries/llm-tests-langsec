#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <hammer/hammer.h>

// Define parsers for HTTP tokens
HParser *create_token_parser(const char *token) {
    return h_token((const uint8_t *)token, strlen(token));
}

HParser *create_digit_parser() {
    return h_ch_range('0', '9');
}

HParser *create_http_version_parser() {
    return h_sequence(
        create_token_parser("HTTP/"),
        create_digit_parser(),
        create_token_parser("."),
        create_digit_parser(),
        NULL
    );
}

HParser *create_general_header_parser() {
    return h_choice(
        create_token_parser("Cache-Control"),
        create_token_parser("Connection"),
        create_token_parser("Date"),
        create_token_parser("Pragma"),
        create_token_parser("Trailer"),
        create_token_parser("Transfer-Encoding"),
        create_token_parser("Upgrade"),
        create_token_parser("Via"),
        NULL
    );
}

// Define parser for allowed characters in tokens
HParser *create_token_char_parser() {
    return h_choice(
        h_ch_range('A', 'Z'),
        h_ch_range('a', 'z'),
        h_ch_range('0', '9'),
        h_ch('-'), h_ch('.'), h_ch('~'), h_ch('_'),
        NULL
    );
}

HParser *create_request_line_parser() {
    return h_sequence(
        h_many1(h_choice(h_ch_range('A', 'Z'), h_ch(' '))),
        h_many1(h_ch(' ')),
        h_many1(create_token_char_parser()),
        h_many1(h_ch(' ')),
        create_http_version_parser(),
        create_token_parser("\r\n"),
        NULL
    );
}

HParser *create_start_line_parser() {
    return h_choice(create_request_line_parser(), NULL);
}

HParser *create_message_header_parser() {
    return h_sequence(
        h_many1(
            h_choice(
                create_general_header_parser(),
                NULL
            )
        ),
        create_token_parser("\r\n"),
        NULL
    );
}

HParser *create_http_message_parser() {
    return h_sequence(
        create_start_line_parser(),
        h_many(create_message_header_parser()),
        create_token_parser("\r\n"),
        h_end_p(),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_binary_file>\n", argv[0]);
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

    uint8_t *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *http_message_parser = create_http_message_parser();
    HParseResult *result = h_parse(http_message_parser, data, file_size);

    if (result) {
        printf("HTTP message parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse HTTP message.\n");
    }

    hobj_unref(http_message_parser);
    free(data);

    return EXIT_SUCCESS;
}