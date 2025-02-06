#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Define parsers for basic components
HParser *http_version_parser() {
    return h_sequence(h_token("HTTP/", 5), h_uint32(), NULL);
}

HParser *status_code_parser() {
    return h_int_range(h_uint8(), 100, 599);
}

HParser *reason_phrase_parser() {
    return h_many1(h_ch_range(32, 126));
}

HParser *header_field_parser() {
    return h_sequence(h_many1(h_ch_range(33, 126)), h_ch(':'), h_whitespace(h_ch(' ')), h_many1(h_ch_range(32, 126)), NULL);
}

HParser *headers_parser() {
    return h_many1(h_sequence(header_field_parser(), h_ch('\r'), h_ch('\n'), NULL));
}

HParser *request_line_parser() {
    return h_sequence(h_many1(h_ch_range(33, 126)), h_whitespace(h_ch(' ')), h_many1(h_ch_range(33, 126)), h_whitespace(h_ch(' ')), http_version_parser(), h_ch('\r'), h_ch('\n'), NULL);
}

HParser *status_line_parser() {
    return h_sequence(http_version_parser(), h_whitespace(h_ch(' ')), status_code_parser(), h_whitespace(h_ch(' ')), reason_phrase_parser(), h_ch('\r'), h_ch('\n'), NULL);
}

HParser *message_body_parser() {
    return h_many1(h_uint8());
}

HParser *http_request_parser() {
    return h_sequence(request_line_parser(), headers_parser(), h_ch('\r'), h_ch('\n'), message_body_parser(), NULL);
}

HParser *http_response_parser() {
    return h_sequence(status_line_parser(), headers_parser(), h_ch('\r'), h_ch('\n'), message_body_parser(), NULL);
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

    HParser *parser = http_request_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result) {
        printf("Parsing successful!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed!\n");
    }

    free(buffer);
    return 0;
}