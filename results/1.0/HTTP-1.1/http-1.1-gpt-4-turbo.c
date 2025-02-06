#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Utility to read input from a file
static char *read_file(const char *filename, size_t *length) {
    FILE *f = fopen(filename, "rb");
    if (!f) {
        perror("Error opening file");
        return NULL;
    }
    fseek(f, 0, SEEK_END);
    *length = ftell(f);
    fseek(f, 0, SEEK_SET);

    char *data = (char *)malloc(*length);
    if (!data) {
        perror("Memory allocation failed");
        fclose(f);
        return NULL;
    }

    if (fread(data, 1, *length, f) != *length) {
        perror("Error reading file");
        fclose(f);
        free(data);
        return NULL;
    }

    fclose(f);
    return data;
}

// Define HTTP 1.1 grammar using Hammer
static HParser *http_grammar() {
    HParser *token = h_token(" ", 1);
    HParser *crlf = h_token("\r\n", 2);
    HParser *colon_space = h_token(": ", 2);

    // Request line: Method SP Request-URI SP HTTP-Version CRLF
    HParser *method = h_choice(h_token("GET", 3), h_token("POST", 4), h_token("HEAD", 4), NULL);
    HParser *request_uri = h_many1(h_not_in(" \r\n", 3));
    HParser *http_version = h_token("HTTP/1.1", 8);
    HParser *request_line = h_sequence(method, token, request_uri, token, http_version, crlf, NULL);

    // Status line: HTTP-Version SP Status-Code SP Reason-Phrase CRLF
    HParser *status_code = h_int_range(h_uint8(), 100, 599);
    HParser *reason_phrase = h_many1(h_not_in("\r\n", 2));
    HParser *status_line = h_sequence(http_version, token, status_code, token, reason_phrase, crlf, NULL);

    // Headers: field-name ":" [ field-value ] CRLF
    HParser *field_name = h_many1(h_not_in(":\r\n", 3));
    HParser *field_value = h_many1(h_not_in("\r\n", 2));
    HParser *header = h_sequence(field_name, colon_space, field_value, crlf, NULL);
    HParser *headers = h_many(header);

    // Message body (arbitrary bytes)
    HParser *message_body = h_greedy1(h_any(), 0);

    // Complete HTTP request and response parsers
    HParser *http_request = h_sequence(request_line, headers, h_optional(message_body), h_end_p(), NULL);
    HParser *http_response = h_sequence(status_line, headers, h_optional(message_body), h_end_p(), NULL);

    return h_choice(http_request, http_response, NULL);
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <filename>\n", argv[0]);
        return 1;
    }

    size_t len;
    char *data = read_file(argv[1], &len);
    if (!data) return 1;

    HParser *parser = http_grammar();
    HParseResult *result = h_parse(parser, (const uint8_t *)data, len);
    if (result) {
        printf("Parse successful!\n");
        h_pprint(stdout, result->ast, 0, 0);
    } else {
        printf("Parse failed.\n");
    }

    h_parse_result_free(result);
    free(data);

    return 0;
}