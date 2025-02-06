#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Define parsers for HTTP fields
HParser *http_version_parser() {
    return h_sequence(h_token((uint8_t*)"HTTP/", 5), h_ch('1'), h_ch('.'), h_ch('1'), NULL);
}

HParser *http_method_parser() {
    return h_choice(h_token((uint8_t*)"GET", 3),
                   h_token((uint8_t*)"POST", 4),
                   h_token((uint8_t*)"PUT", 3),
                   h_token((uint8_t*)"DELETE", 6),
                   h_token((uint8_t*)"HEAD", 4),
                   h_token((uint8_t*)"OPTIONS", 7),
                   h_token((uint8_t*)"TRACE", 5),
                   h_token((uint8_t*)"CONNECT", 7),
                   NULL);
}

HParser *http_uri_parser() {
    return h_many1(h_ch_range(32, 126)); // Printable ASCII characters
}

HParser *http_header_field_parser() {
    return h_sequence(h_many1(h_ch_range('a', 'z')), h_token((uint8_t*)": ", 2), h_many1(h_ch_range(32, 126)), NULL);
}

HParser *http_request_line_parser() {
    return h_sequence(http_method_parser(), h_ch(' '), http_uri_parser(), h_ch(' '), http_version_parser(), h_ch('\r'), h_ch('\n'), NULL);
}

HParser *http_headers_parser() {
    return h_many(http_header_field_parser());
}

HParser *http_request_parser() {
    return h_sequence(http_request_line_parser(), http_headers_parser(), h_ch('\r'), h_ch('\n'), NULL);
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

    uint8_t *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParseResult *result = h_parse(http_request_parser(), data, file_size);
    if (result) {
        printf("Parsing successful!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed.\n");
    }

    free(data);
    return 0;
}