#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Define parsers for basic types
HParser *http_version_parser() {
    return h_sequence(h_token("HTTP/", 5), h_repeat_n(h_ch_range('0', '9'), 1), h_ch('.'), h_repeat_n(h_ch_range('0', '9'), 1), NULL);
}

HParser *http_method_parser() {
    return h_choice(h_token("GET", 3), h_token("POST", 4), h_token("PUT", 3), h_token("DELETE", 6), h_token("HEAD", 4), h_token("OPTIONS", 7), NULL);
}

HParser *http_uri_parser() {
    return h_many1(h_choice(h_ch_range('a', 'z'), h_ch_range('0', '9'), NULL));
}

HParser *http_status_code_parser() {
    return h_int_range(h_repeat_n(h_ch_range('0', '9'), 3), 100, 599);
}

HParser *http_reason_phrase_parser() {
    return h_many1(h_choice(h_ch_range('a', 'z'), h_whitespace(h_any()), NULL));
}

HParser *http_header_field_parser() {
    return h_sequence(h_many1(h_ch_range('a', 'z')), h_ch(':'), h_whitespace(h_any()), h_many1(h_choice(h_ch_range('a', 'z'), h_ch_range('0', '9'), NULL)), NULL);
}

HParser *http_message_body_parser() {
    return h_many(h_any());
}

HParser *http_request_line_parser() {
    return h_sequence(http_method_parser(), h_whitespace(h_any()), http_uri_parser(), h_whitespace(h_any()), http_version_parser(), h_ch('\r'), h_ch('\n'), NULL);
}

HParser *http_status_line_parser() {
    return h_sequence(http_version_parser(), h_whitespace(h_any()), http_status_code_parser(), h_whitespace(h_any()), http_reason_phrase_parser(), h_ch('\r'), h_ch('\n'), NULL);
}

HParser *http_header_parser() {
    return h_many(h_sequence(http_header_field_parser(), h_ch('\r'), h_ch('\n'), NULL));
}

HParser *http_message_parser() {
    return h_sequence(http_request_line_parser(), http_header_parser(), h_ch('\r'), h_ch('\n'), http_message_body_parser(), NULL);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = (uint8_t *)malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = http_message_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result) {
        printf("Parsing succeeded!\n");
        h_pprint(stdout, result->ast, 0, 4);
        h_parse_result_free(result);
    } else {
        printf("Parsing failed.\n");
    }

    free(buffer);
    return 0;
}