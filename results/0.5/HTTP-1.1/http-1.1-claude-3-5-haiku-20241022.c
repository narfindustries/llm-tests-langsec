#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <hammer/hammer.h>

typedef struct {
    HParser* method;
    HParser* uri;
    HParser* version;
    HParser* headers;
    HParser* body;
} HTTPParser;

HParser* http_method() {
    return h_choice(
        h_string("GET"),
        h_string("POST"),
        h_string("PUT"),
        h_string("DELETE"),
        h_string("HEAD"),
        h_string("OPTIONS"),
        h_string("TRACE"),
        h_string("CONNECT"),
        h_string("PATCH"),
        NULL
    );
}

HParser* http_version() {
    return h_sequence(
        h_string("HTTP/"),
        h_choice(
            h_string("1.0"),
            h_string("1.1"),
            NULL
        ),
        NULL
    );
}

HParser* http_header_name() {
    return h_many1(h_satisfy_char(h_in_charset("A-Za-z0-9-_")));
}

HParser* http_header_value() {
    return h_many1(h_satisfy_char(h_not_in_charset("\r\n")));
}

HParser* http_header() {
    return h_sequence(
        http_header_name(),
        h_ch(':'),
        h_whitespace_parser(),
        http_header_value(),
        h_ch('\r'),
        h_ch('\n'),
        NULL
    );
}

HParser* http_headers() {
    return h_many(http_header());
}

HParser* http_request_line() {
    return h_sequence(
        http_method(),
        h_whitespace_parser(),
        h_many1(h_satisfy_char(h_not_in_charset(" \r\n"))),
        h_whitespace_parser(),
        http_version(),
        h_ch('\r'),
        h_ch('\n'),
        NULL
    );
}

HParser* http_request() {
    return h_sequence(
        http_request_line(),
        http_headers(),
        h_ch('\r'),
        h_ch('\n'),
        NULL
    );
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }

    size_t read_size = fread(buffer, 1, file_size, file);
    fclose(file);

    if (read_size != file_size) {
        perror("File read error");
        free(buffer);
        return 1;
    }

    HParser* parser = http_request();
    HParseResult* result = h_parse(parser, buffer, read_size);

    if (result && result->ast) {
        printf("HTTP request parsed successfully\n");
    } else {
        printf("HTTP request parsing failed\n");
    }

    h_parse_result_free(result);
    h_destroy_parser(parser);
    free(buffer);

    return 0;
}