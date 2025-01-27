#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Define the HTTP/1.1 grammar using Hammer
HParser *http_grammar() {
    H_RULE(method, h_choice(
        h_token("GET"),
        h_token("POST"),
        h_token("PUT"),
        h_token("DELETE"),
        h_token("HEAD"),
        h_token("OPTIONS"),
        h_token("CONNECT"),
        h_token("TRACE"),
        h_token("PATCH"),
        NULL
    ));

    H_RULE(space, h_ch(' '));
    H_RULE(crlf, h_sequence(h_ch('\r'), h_ch('\n'), NULL));
    H_RULE(version, h_sequence(h_ch('H'), h_ch('T'), h_ch('T'), h_ch('P'), h_ch('/'), h_ch('1'), h_ch('.'), h_ch('1'), NULL));
    H_RULE(request_line, h_sequence(method, space, h_until(h_ch(' ')), space, version, crlf, NULL));
    H_RULE(header, h_sequence(h_until(h_ch(':')), h_ch(':'), h_optional(h_ch(' ')), h_until(crlf), crlf, NULL));
    H_RULE(headers, h_many(header));
    H_RULE(body, h_many(h_any()));
    H_RULE(http_request, h_sequence(request_line, headers, crlf, body, NULL));

    return http_request;
}

int main(int argc, char **argv) {
    HParser *parser = http_grammar();
    const char *input = "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n";
    HParseResult *result = h_parse(parser, (const uint8_t*)input, strlen(input));

    if (result) {
        printf("Parsing succeeded!\n");
        h_pprint(stdout, result->ast, 0, 0);
        h_parse_result_free(result);
    } else {
        printf("Parsing failed.\n");
    }

    h_parser_free(parser);
    return 0;
}