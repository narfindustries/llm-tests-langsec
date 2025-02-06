#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define parsers for various HTTP header components
HParser *http_version, *method, *uri, *status_code, *reason_phrase;
HParser *request_line, *status_line;
HParser *header_field, *header_fields;
HParser *http_message;

void init_parsers() {
    http_version = h_sequence(h_ch('H'), h_ch('T'), h_ch('T'), h_ch('P'), h_ch('/'), h_bits(4, false), h_ch('.'), h_bits(4, false), NULL);

    method = h_choice(h_string("GET"), h_string("POST"), h_string("HEAD"), h_string("PUT"),
                      h_string("DELETE"), h_string("CONNECT"), h_string("OPTIONS"), h_string("TRACE"), NULL);

    uri = h_many1(h_not_char(' '));

    status_code = h_bits(8, false);

    reason_phrase = h_many(h_not_char('\r'));

    request_line = h_sequence(method, h_ch(' '), uri, h_ch(' '), http_version, h_ch('\r'), h_ch('\n'), NULL);

    status_line = h_sequence(http_version, h_ch(' '), status_code, h_ch(' '), reason_phrase, h_ch('\r'), h_ch('\n'), NULL);

    header_field = h_sequence(h_many1(h_not_char(':')), h_ch(':'), h_ch(' '), h_many(h_not_char('\r')), h_ch('\r'), h_ch('\n'), NULL);

    header_fields = h_many(header_field);

    http_message = h_sequence(h_choice(request_line, status_line, NULL), header_fields, h_many1(h_ch('\r')), h_many1(h_ch('\n')), NULL);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <file>\n", argv[0]);
        return 1;
    }

    FILE *f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    long fsize = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t *data = malloc(fsize);
    fread(data, 1, fsize, f);

    fclose(f);

    init_parsers();

    HParseResult *result = h_parse(http_message, data, fsize);
    if (result) {
        printf("Parsed successfully.\n");
    } else {
        printf("Failed to parse.\n");
    }

    free(data);
    return 0;
}