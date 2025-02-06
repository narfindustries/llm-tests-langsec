#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Define parsers for various HTTP/1.1 components
HParser *create_token_parser() {
    return h_many1(h_choice(h_ch_range('A', 'Z'), h_ch_range('a', 'z'), h_ch_range('0', '9'), h_ch('-'), h_ch('_'), h_ch('.'), NULL));
}

HParser *create_http_version_parser() {
    return h_sequence(h_token("HTTP/", 5), h_uint8(), h_ch('.'), h_uint8(), NULL);
}

HParser *create_header_field_parser() {
    HParser *field_name = create_token_parser();
    HParser *field_value = h_many(h_choice(h_ch_range(0x20, 0x7E), h_ch('\t'), NULL));
    return h_sequence(field_name, h_whitespace(h_ch(' ')), h_ch(':'), h_whitespace(h_ch(' ')), field_value, h_ch('\r'), h_ch('\n'), NULL);
}

HParser *create_request_line_parser() {
    HParser *method = create_token_parser();
    HParser *request_target = h_many1(h_choice(h_ch_range(0x21, 0x7E), h_ch('%'), NULL));
    HParser *http_version = create_http_version_parser();
    return h_sequence(method, h_whitespace(h_ch(' ')), request_target, h_whitespace(h_ch(' ')), http_version, h_ch('\r'), h_ch('\n'), NULL);
}

HParser *create_status_line_parser() {
    HParser *http_version = create_http_version_parser();
    HParser *status_code = h_repeat_n(h_ch_range('0', '9'), 3);
    HParser *reason_phrase = h_many(h_ch_range(0x20, 0x7E));
    return h_sequence(http_version, h_whitespace(h_ch(' ')), status_code, h_whitespace(h_ch(' ')), reason_phrase, h_ch('\r'), h_ch('\n'), NULL);
}

HParser *create_message_body_parser() {
    return h_many(h_ch_range(0x00, 0xFF));
}

HParser *create_http_message_parser() {
    HParser *start_line = h_choice(create_request_line_parser(), create_status_line_parser(), NULL);
    HParser *header_fields = h_many(create_header_field_parser());
    HParser *message_body = create_message_body_parser();
    return h_sequence(start_line, header_fields, h_ch('\r'), h_ch('\n'), message_body, NULL);
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

    HParser *http_message_parser = create_http_message_parser();
    HParseResult *result = h_parse(http_message_parser, data, file_size);

    if (result) {
        printf("HTTP message parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse HTTP message.\n");
    }

    free(data);
    return EXIT_SUCCESS;
}