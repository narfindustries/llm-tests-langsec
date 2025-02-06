#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

HParser* http_request;
HParser* http_response;
HParser* http_message;

HParser* sp;
HParser* crlf;
HParser* token;
HParser* quoted_string;
HParser* method;
HParser* http_version;
HParser* status_code;

HParser* create_http_parsers() {
    // Basic character parsers
    const uint8_t sp_chars[] = {' '};
    const uint8_t token_exclude[] = {' ', '\r', '\n', '\t', '(', ')', '{', '}', '[', ']', '<', '>', ':', ';', ',', '"', '/', '?', '='};
    const uint8_t quote_exclude[] = {'"'};
    const uint8_t crlf_chars[] = {'\r', '\n'};

    sp = h_ch(sp_chars[0]);
    crlf = h_sequence(h_ch('\r'), h_ch('\n'), NULL);
    token = h_many1(h_not_in(token_exclude, sizeof(token_exclude)));
    quoted_string = h_sequence(
        h_ch('"'),
        h_many(h_not_in(quote_exclude, sizeof(quote_exclude))),
        h_ch('"'),
        NULL
    );

    // HTTP Method parser
    method = h_choice(
        h_parse_string("GET"),
        h_parse_string("POST"),
        h_parse_string("PUT"),
        h_parse_string("DELETE"),
        h_parse_string("HEAD"),
        h_parse_string("OPTIONS"),
        h_parse_string("TRACE"),
        h_parse_string("CONNECT"),
        NULL
    );

    // HTTP Version parser
    http_version = h_sequence(
        h_parse_string("HTTP/"),
        h_choice(
            h_parse_string("1.0"),
            h_parse_string("1.1"),
            NULL
        ),
        NULL
    );

    // Status Code parser
    HParser* zero = h_ch('0');
    HParser* nine = h_ch('9');
    HParser* digit = h_in_range(zero, nine);
    status_code = h_sequence(
        digit, digit, digit,
        NULL
    );

    // Headers parser
    HParser* header_name = token;
    HParser* header_value = h_many1(h_not_in(crlf_chars, sizeof(crlf_chars)));
    
    HParser* header = h_sequence(
        header_name,
        h_ch(':'),
        sp,
        header_value,
        crlf,
        NULL
    );

    HParser* headers = h_many(header);

    // Request Line parser
    HParser* request_line = h_sequence(
        method, sp,
        token, sp,  // Request URI
        http_version,
        crlf,
        NULL
    );

    // Response Line parser
    HParser* response_line = h_sequence(
        http_version, sp,
        status_code, sp,
        h_many(h_not_in(crlf_chars, sizeof(crlf_chars))),  // Reason Phrase
        crlf,
        NULL
    );

    // Message Body parser
    const uint8_t null_char[] = {0};
    HParser* message_body = h_many(h_not_in(null_char, sizeof(null_char)));

    // Complete Request parser
    http_request = h_sequence(
        request_line,
        headers,
        crlf,
        h_optional(message_body),
        NULL
    );

    // Complete Response parser
    http_response = h_sequence(
        response_line,
        headers,
        crlf,
        h_optional(message_body),
        NULL
    );

    // Generic Message parser
    http_message = h_choice(
        http_request,
        http_response,
        NULL
    );

    return http_message;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <http_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t *buffer = malloc(file_size);
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

    create_http_parsers();
    HParseResult* result = h_parse(http_message, buffer, read_size);

    if (result && result->ast) {
        printf("Parsing successful!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed.\n");
    }

    free(buffer);
    return 0;
}