#include <hammer/hammer.h>
#include <hammer/glue.h>

// Define the basic building blocks of HTTP 1.1
static HParser *token;
static HParser *CRLF;
static HParser *SP;
static HParser *colon;
static HParser *OWS; // Optional whitespace

// HTTP components
static HParser *request_line;
static HParser *status_line;
static HParser *header_field;
static HParser *message_body;
static HParser *http_version;
static HParser *method;
static HParser *request_target;
static HParser *reason_phrase;
static HParser *status_code;
static HParser *headers;
static HParser *http_message;

void init_parser() {
    // Basic tokens as per the HTTP/1.1 spec
    SP = h_ch(' ');
    colon = h_ch(':');
    CRLF = h_sequence(h_ch('\r'), h_ch('\n'), NULL);
    OWS = h_optional(h_many(h_ch(' ')));

    // Token definitions
    token = h_many1(h_ch_range(0x21, 0x7E));

    // HTTP version
    http_version = h_sequence(h_string("HTTP/"), h_int_range(h_uint8(), 0, 1), h_ch('.'), h_int_range(h_uint8(), 0, 1), NULL);

    // Request line components
    method = token;
    request_target = token;
    request_line = h_sequence(method, SP, request_target, SP, http_version, CRLF, NULL);

    // Status line components
    status_code = h_int_range(h_uint16(), 100, 599);
    reason_phrase = h_many(h_ch_range(0x20, 0x7E)); // Reason-Phrase characters
    status_line = h_sequence(http_version, SP, status_code, SP, reason_phrase, CRLF, NULL);

    // Header field
    header_field = h_sequence(token, colon, OWS, token, OWS, CRLF, NULL);

    // Headers
    headers = h_many(header_field);

    // Message body
    message_body = h_many(h_any());

    // Complete HTTP message
    http_message = h_sequence(h_choice(request_line, status_line, NULL), headers, h_optional(message_body), NULL);
}

int main(int argc, char **argv) {
    init_parser();

    // Example usage
    uint8_t input[] = "GET /index.html HTTP/1.1\r\nHost: example.com\r\n\r\n";
    size_t input_len = sizeof(input) - 1;
    HParseResult *result = h_parse(http_message, input, input_len);
    if (result) {
        printf("Parse successful!\n");
    } else {
        printf("Parse failed.\n");
    }

    return 0;
}