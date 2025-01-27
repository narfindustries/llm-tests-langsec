#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Define the HTTP/1.1 grammar using Hammer
HParser *http_grammar() {
    // Define basic tokens
    HParser *crlf = h_ch('\r');
    HParser *lf = h_ch('\n');
    HParser *space = h_ch(' ');
    HParser *colon = h_ch(':');
    HParser *slash = h_ch('/');
    HParser *http_version = h_sequence(h_ch('H'), h_ch('T'), h_ch('T'), h_ch('P'), h_ch('/'), h_ch('1'), h_ch('.'), h_ch('1'), NULL);
    HParser *method = h_choice(h_token("GET"), h_token("POST"), h_token("PUT"), h_token("DELETE"), NULL);
    HParser *uri = h_many1(h_choice(h_alpha(), h_digit(), h_ch('-'), h_ch('.'), h_ch('_'), h_ch('~'), h_ch(':'), h_ch('/'), h_ch('?'), h_ch('#'), h_ch('['), h_ch(']'), h_ch('@'), h_ch('!'), h_ch('$'), h_ch('&'), h_ch('\''), h_ch('('), h_ch(')'), h_ch('*'), h_ch('+'), h_ch(','), h_ch(';'), h_ch('='), NULL));
    HParser *header_name = h_many1(h_choice(h_alpha(), h_digit(), h_ch('-'), NULL));
    HParser *header_value = h_many1(h_choice(h_alpha(), h_digit(), h_ch('-'), h_ch('.'), h_ch('_'), h_ch('~'), h_ch(':'), h_ch('/'), h_ch('?'), h_ch('#'), h_ch('['), h_ch(']'), h_ch('@'), h_ch('!'), h_ch('$'), h_ch('&'), h_ch('\''), h_ch('('), h_ch(')'), h_ch('*'), h_ch('+'), h_ch(','), h_ch(';'), h_ch('='), h_ch(' '), NULL));
    HParser *header = h_sequence(header_name, colon, space, header_value, NULL);
    HParser *headers = h_many(h_sequence(header, crlf, lf, NULL));
    HParser *request_line = h_sequence(method, space, uri, space, http_version, crlf, lf, NULL);
    HParser *http_request = h_sequence(request_line, headers, crlf, lf, NULL);

    return http_request;
}

int main() {
    // Create the HTTP/1.1 parser
    HParser *parser = http_grammar();

    // Example HTTP request
    const char *http_request = "GET /index.html HTTP/1.1\r\n"
                               "Host: www.example.com\r\n"
                               "User-Agent: Mozilla/5.0\r\n"
                               "\r\n";

    // Parse the HTTP request
    HParseResult *result = h_parse(parser, (const uint8_t*)http_request, strlen(http_request));

    if (result) {
        printf("Parsing successful!\n");
        h_pprint(stdout, result->ast, 0, 0);
        h_parse_result_free(result);
    } else {
        printf("Parsing failed!\n");
    }

    // Clean up
    h_parser_free(parser);

    return 0;
}