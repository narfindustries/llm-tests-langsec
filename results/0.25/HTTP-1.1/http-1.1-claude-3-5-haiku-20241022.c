#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <hammer/hammer.h>

typedef struct {
    HParser* method;
    HParser* uri;
    HParser* version;
    HParser* headers;
    HParser* body;
} HTTPRequest;

typedef struct {
    HParser* status_code;
    HParser* status_text;
    HParser* version;
    HParser* headers;
    HParser* body;
} HTTPResponse;

HParser* http_method() {
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

HParser* http_version() {
    return h_sequence(
        h_literal_str("HTTP/"),
        h_choice(
            h_literal_str("1.0"),
            h_literal_str("1.1"),
            NULL
        ),
        NULL
    );
}

HParser* header_name() {
    return h_many1(h_satisfy_charclass(isprint));
}

HParser* header_value() {
    return h_many1(h_satisfy_charclass(isprint));
}

HParser* http_header() {
    return h_sequence(
        header_name(),
        h_ch(':'),
        h_ch(' '),
        header_value(),
        h_ch('\r'),
        h_ch('\n'),
        NULL
    );
}

HParser* http_headers() {
    return h_many(http_header());
}

HParser* http_request() {
    return h_sequence(
        http_method(),
        h_whitespace(h_ch(' ')),
        h_many1(h_satisfy_charclass(isprint)),
        h_whitespace(h_ch(' ')),
        http_version(),
        h_ch('\r'),
        h_ch('\n'),
        http_headers(),
        h_ch('\r'),
        h_ch('\n'),
        h_optional(h_many1(h_satisfy_charclass(isprint))),
        NULL
    );
}

HParser* http_response() {
    return h_sequence(
        http_version(),
        h_whitespace(h_ch(' ')),
        h_many1(h_satisfy_charclass(isdigit)),
        h_whitespace(h_ch(' ')),
        h_many1(h_satisfy_charclass(isprint)),
        h_ch('\r'),
        h_ch('\n'),
        http_headers(),
        h_ch('\r'),
        h_ch('\n'),
        h_optional(h_many1(h_satisfy_charclass(isprint))),
        NULL
    );
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <http_file>\n", argv[0]);
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

    HParser* request_parser = http_request();
    HParser* response_parser = http_response();

    HParseResult* request_result = h_parse(request_parser, buffer, read_size);
    HParseResult* response_result = h_parse(response_parser, buffer, read_size);

    if (request_result && request_result->ast) {
        printf("Valid HTTP Request\n");
    } else if (response_result && response_result->ast) {
        printf("Valid HTTP Response\n");
    } else {
        printf("Invalid HTTP Message\n");
    }

    h_parse_result_free(request_result);
    h_parse_result_free(response_result);
    h_destroy(request_parser);
    h_destroy(response_parser);
    free(buffer);

    return 0;
}