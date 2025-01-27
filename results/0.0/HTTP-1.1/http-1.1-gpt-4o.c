#include <hammer/hammer.h>

HParser *create_http_parser() {
    // Define basic tokens
    HParser *SP = h_ch(' ');
    HParser *HTAB = h_ch('\t');
    HParser *CRLF = h_sequence(h_ch('\r'), h_ch('\n'), NULL);
    HParser *LWS = h_many1(h_choice(SP, HTAB, CRLF, NULL));

    // Define HTTP version
    HParser *HTTP_Version = h_sequence(
        h_token("HTTP/", 5),
        h_uint(1, 10), // Major version
        h_ch('.'),
        h_uint(1, 10), // Minor version
        NULL
    );

    // Define request line
    HParser *Method = h_choice(
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

    HParser *Request_URI = h_many1(h_choice(
        h_range('a', 'z'),
        h_range('A', 'Z'),
        h_range('0', '9'),
        h_ch('/'),
        h_ch('.'),
        h_ch('-'),
        h_ch('_'),
        h_ch('~'),
        h_ch('%'),
        h_ch('?'),
        h_ch('&'),
        h_ch('='),
        h_ch('#'),
        NULL
    ));

    HParser *Request_Line = h_sequence(
        Method,
        LWS,
        Request_URI,
        LWS,
        HTTP_Version,
        CRLF,
        NULL
    );

    // Define header fields
    HParser *Token = h_many1(h_choice(
        h_range('a', 'z'),
        h_range('A', 'Z'),
        h_range('0', '9'),
        h_ch('-'),
        h_ch('_'),
        NULL
    ));

    HParser *Header_Value = h_many(h_choice(
        h_range(0x20, 0x7E), // Visible ASCII characters
        h_range(0x80, 0xFF), // Extended ASCII characters
        NULL
    ));

    HParser *Header_Field = h_sequence(
        Token,
        h_ch(':'),
        LWS,
        Header_Value,
        CRLF,
        NULL
    );

    HParser *Header_Fields = h_many(Header_Field);

    // Define HTTP message
    HParser *HTTP_Message = h_sequence(
        Request_Line,
        Header_Fields,
        CRLF, // End of headers
        NULL
    );

    return HTTP_Message;
}

int main() {
    HParser *http_parser = create_http_parser();
    // Use the parser with some input data
    // Example: HParseResult *result = h_parse(http_parser, input_data, input_length);
    h_delete(http_parser);
    return 0;
}