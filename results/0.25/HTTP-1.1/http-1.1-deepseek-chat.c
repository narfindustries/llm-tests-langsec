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
    HParser *dot = h_ch('.');
    HParser *http_version = h_sequence(h_string("HTTP/"), h_uint8(), h_ch('.'), h_uint8(), NULL);
    
    // Define HTTP methods
    HParser *method = h_choice(
        h_string("GET"),
        h_string("POST"),
        h_string("PUT"),
        h_string("DELETE"),
        h_string("HEAD"),
        h_string("OPTIONS"),
        h_string("CONNECT"),
        h_string("TRACE"),
        h_string("PATCH"),
        NULL
    );
    
    // Define URI
    HParser *uri = h_many1(h_choice(h_alnum(), h_ch('/'), h_ch('.'), h_ch('-'), h_ch('_'), h_ch('~'), h_ch(':'), h_ch('?'), h_ch('#'), h_ch('['), h_ch(']'), h_ch('@'), h_ch('!'), h_ch('$'), h_ch('&'), h_ch('\''), h_ch('('), h_ch(')'), h_ch('*'), h_ch('+'), h_ch(','), h_ch(';'), h_ch('='), NULL));
    
    // Define HTTP header
    HParser *header_name = h_many1(h_choice(h_alpha(), h_ch('-'), NULL));
    HParser *header_value = h_many1(h_choice(h_alnum(), h_ch(' '), h_ch('-'), h_ch('.'), h_ch(','), h_ch(';'), h_ch(':'), h_ch('='), h_ch('@'), h_ch('/'), h_ch('?'), h_ch('!'), h_ch('#'), h_ch('$'), h_ch('&'), h_ch('\''), h_ch('('), h_ch(')'), h_ch('*'), h_ch('+'), h_ch(','), h_ch(';'), h_ch('='), NULL));
    HParser *header = h_sequence(header_name, colon, h_optional(space), header_value, NULL);
    
    // Define HTTP headers
    HParser *headers = h_many(h_sequence(header, crlf, lf, NULL));
    
    // Define HTTP request line
    HParser *request_line = h_sequence(method, space, uri, space, http_version, crlf, lf, NULL);
    
    // Define HTTP request
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
                               "Accept: text/html\r\n"
                               "\r\n";
    
    // Parse the HTTP request
    HParseResult *result = h_parse(parser, (const uint8_t*)http_request, strlen(http_request));
    
    if (result) {
        printf("Parsing successful!\n");
        h_pprint(result->ast, stdout, 0);
        h_parse_result_free(result);
    } else {
        printf("Parsing failed!\n");
    }
    
    return 0;
}