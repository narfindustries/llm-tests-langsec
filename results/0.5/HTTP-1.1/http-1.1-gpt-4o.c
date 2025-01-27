#include <hammer/hammer.h>

HParser *create_http_parser() {
    HParser *method = h_choice(
        h_token("GET", 3),
        h_token("POST", 4),
        h_token("PUT", 3),
        h_token("DELETE", 6),
        h_token("HEAD", 4),
        h_token("OPTIONS", 7),
        h_token("PATCH", 5),
        h_token("TRACE", 5),
        h_token("CONNECT", 7),
        NULL
    );

    HParser *space = h_whitespace();
    HParser *http_version = h_sequence(
        h_token("HTTP/", 5),
        h_uint(1), // Major version
        h_token(".", 1),
        h_uint(1), // Minor version
        NULL
    );

    HParser *header_name = h_many1(h_choice(h_range('a', 'z'), h_range('A', 'Z'), h_range('0', '9'), h_token("-", 1), NULL));
    HParser *header_value = h_many(h_choice(h_range(32, 126), h_token("\t", 1), NULL)); // ASCII printable characters and tab
    HParser *header = h_sequence(header_name, h_token(":", 1), space, header_value, NULL);
    HParser *headers = h_many(h_sequence(header, h_token("\r\n", 2), NULL));

    HParser *request_line = h_sequence(method, space, h_many1(h_choice(h_range(33, 126), h_token("\t", 1), NULL)), space, http_version, h_token("\r\n", 2), NULL);
    HParser *http_request = h_sequence(request_line, headers, h_token("\r\n", 2), NULL);

    return http_request;
}

int main(int argc, char **argv) {
    HParser *http_parser = create_http_parser();
    // Example usage of the parser will go here
    // Clean up
    h_parser_free(http_parser);
    return 0;
}