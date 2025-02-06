#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Helper functions to create parsers
HParser *create_SP() {
    return h_many1(h_ch(' '));
}

HParser *create_CRLF() {
    return h_sequence(h_ch('\r'), h_ch('\n'), NULL);
}

HParser *create_token() {
    return h_many1(h_choice(h_ch_range('a', 'z'), h_ch_range('A', 'Z'), h_ch_range('0', '9'), h_ch_any("!#$%&'*+-.^_`|~"), NULL));
}

HParser *create_header_value() {
    return h_many1(h_choice(h_ch_range(32, 126), h_ch('\t'), NULL)); // Visible ASCII and tab
}

HParser *create_http_version() {
    return h_sequence(h_ch('H'), h_ch('T'), h_ch('T'), h_ch('P'), h_ch('/'), h_ch_range('0', '9'), h_ch('.'), h_ch_range('0', '9'), NULL);
}

HParser *create_status_code() {
    return h_repeat_n(h_ch_range('0', '9'), 3);
}

HParser *create_reason_phrase() {
    return h_many1(h_choice(h_ch_range(32, 126), h_ch('\t'), NULL)); // Visible ASCII and tab
}

// Main HTTP parser
HParser *create_http_parser() {
    HParser *SP = create_SP();
    HParser *CRLF = create_CRLF();
    HParser *token = create_token();
    HParser *header_name = token;
    HParser *header_value = create_header_value();
    HParser *header = h_sequence(header_name, h_ch(':'), SP, header_value, CRLF, NULL);
    HParser *headers = h_many(header);

    HParser *method = token;
    HParser *request_uri = h_many1(h_choice(h_ch_range('a', 'z'), h_ch_range('A', 'Z'), h_ch_range('0', '9'), h_ch_any("!#$%&'*+-.^_`|~:/?#[]@!$&'()*+,;="), NULL));
    HParser *http_version = create_http_version();
    HParser *request_line = h_sequence(method, SP, request_uri, SP, http_version, CRLF, NULL);

    HParser *status_code = create_status_code();
    HParser *reason_phrase = create_reason_phrase();
    HParser *status_line = h_sequence(http_version, SP, status_code, SP, reason_phrase, CRLF, NULL);

    return h_choice(
        h_sequence(request_line, headers, CRLF, NULL), // Request
        h_sequence(status_line, headers, CRLF, NULL)  // Response
    );
}

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

    HParser *http_message = create_http_parser();
    HParseResult *result = h_parse(http_message, data, file_size);
    if (result) {
        printf("HTTP message parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse HTTP message.\n");
    }

    free(data);
    return EXIT_SUCCESS;
}