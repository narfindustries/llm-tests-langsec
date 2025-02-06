#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    HParsedToken **headers;
    size_t headers_len;
    char *method;
    char *uri;
    char *version;
    uint8_t *body;
    size_t body_len;
} HTTPRequest;

typedef struct {
    int status_code;
    char *status_text;
    HParsedToken **headers;
    size_t headers_len;
    uint8_t *body;
    size_t body_len;
} HTTPResponse;

static HParser* parse_method(void) {
    return h_choice(
        h_literal_str("GET"),
        h_literal_str("POST"),
        h_literal_str("PUT"),
        h_literal_str("DELETE"),
        h_literal_str("HEAD"),
        h_literal_str("OPTIONS"),
        h_literal_str("TRACE"),
        h_literal_str("CONNECT"),
        h_literal_str("PATCH"),
        NULL
    );
}

static HParser* parse_uri(void) {
    return h_many1(h_ch_range('!', '~'));
}

static HParser* parse_http_version(void) {
    return h_sequence(
        h_literal_str("HTTP/"),
        h_choice(h_literal_str("1.0"), h_literal_str("1.1")),
        NULL
    );
}

static HParser* parse_header_name(void) {
    return h_many1(h_ch_range('!', '~'));
}

static HParser* parse_header_value(void) {
    return h_many1(h_ch_range(' ', '~'));
}

static HParser* parse_header(void) {
    return h_sequence(
        parse_header_name(),
        h_ch(':'),
        h_optional(h_ch(' ')),
        parse_header_value(),
        h_literal_str("\r\n"),
        NULL
    );
}

static HParser* parse_headers(void) {
    return h_many(parse_header());
}

static HParser* parse_body(void) {
    return h_many1(h_ch_range(0, 255));
}

static HParser* parse_request(void) {
    return h_sequence(
        parse_method(),
        h_ch(' '),
        parse_uri(),
        h_ch(' '),
        parse_http_version(),
        h_literal_str("\r\n"),
        parse_headers(),
        h_literal_str("\r\n"),
        h_optional(parse_body()),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Could not open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation failed");
        fclose(file);
        return 1;
    }

    if (fread(buffer, 1, file_size, file) != file_size) {
        perror("File read error");
        free(buffer);
        fclose(file);
        return 1;
    }
    fclose(file);

    HParser *parser = parse_request();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result && result->ast) {
        printf("Parsing successful\n");
    } else {
        printf("Parsing failed\n");
    }

    h_parse_result_free(result);
    h_destroy(parser);
    free(buffer);
    return 0;
}