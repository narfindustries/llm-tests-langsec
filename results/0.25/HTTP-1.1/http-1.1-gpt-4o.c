#include <hammer/hammer.h>

HParser *create_http_parser() {
    // Define basic tokens
    HParser *SP = h_ch(' ');
    HParser *CRLF = h_sequence(h_ch('\r'), h_ch('\n'), NULL);
    HParser *DIGIT = h_range('0', '9');
    HParser *ALPHA = h_choice(h_range('a', 'z'), h_range('A', 'Z'), NULL);
    HParser *TOKEN = h_many1(h_choice(ALPHA, DIGIT, h_ch('-'), h_ch('_'), NULL));

    // Define HTTP version
    HParser *HTTP_VERSION = h_sequence(
        h_string("HTTP/"),
        DIGIT,
        h_ch('.'),
        DIGIT,
        NULL
    );

    // Define request line
    HParser *METHOD = TOKEN;
    HParser *REQUEST_TARGET = h_many1(h_choice(ALPHA, DIGIT, h_ch('/'), h_ch('?'), h_ch('&'), h_ch('='), h_ch('%'), h_ch('-'), h_ch('_'), h_ch('.'), NULL));
    HParser *REQUEST_LINE = h_sequence(
        METHOD,
        SP,
        REQUEST_TARGET,
        SP,
        HTTP_VERSION,
        CRLF,
        NULL
    );

    // Define header fields
    HParser *FIELD_NAME = TOKEN;
    HParser *FIELD_VALUE = h_many(h_choice(ALPHA, DIGIT, SP, h_ch('-'), h_ch('_'), h_ch(':'), h_ch('/'), h_ch('.'), h_ch(','), h_ch(';'), h_ch('='), h_ch('?'), h_ch('&'), h_ch('%'), NULL));
    HParser *HEADER_FIELD = h_sequence(
        FIELD_NAME,
        h_ch(':'),
        SP,
        FIELD_VALUE,
        CRLF,
        NULL
    );

    // Define headers
    HParser *HEADERS = h_many(HEADER_FIELD);

    // Define HTTP message
    HParser *HTTP_MESSAGE = h_sequence(
        REQUEST_LINE,
        HEADERS,
        CRLF, // End of headers
        NULL
    );

    return HTTP_MESSAGE;
}

int main(int argc, char **argv) {
    HParser *http_parser = create_http_parser();
    // Use the parser with input data here
    h_parser_free(http_parser);
    return 0;
}