#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define HAMMER_NO_PARSER

typedef struct {
    char* method;
    char* request_uri;
    char* http_version;
} request_line_t;

typedef struct {
    char* accept;
    char* accept_charset;
    char* accept_encoding;
    char* accept_language;
    char* authorization;
    char* cache_control;
    char* connection;
    char* content_length;
    char* content_type;
    char* date;
    char* expect;
    char* from;
    char* host;
    char* if_match;
    char* if_modified_since;
    char* if_none_match;
    char* if_range;
    char* if_unmodified_since;
    char* max_forwards;
    char* proxy_authorization;
    char* range;
    char* referer;
    char* te;
    char* user_agent;
} request_headers_t;

typedef struct {
    char* http_version;
    int status_code;
    char* reason_phrase;
} status_line_t;

typedef struct {
    char* accept_ranges;
    char* age;
    char* allow;
    char* cache_control;
    char* connection;
    char* content_encoding;
    char* content_language;
    char* content_length;
    char* content_location;
    char* content_md5;
    char* content_range;
    char* content_type;
    char* date;
    char* etag;
    char* expires;
    char* last_modified;
    char* location;
    char* proxy_authenticate;
    char* retry_after;
    char* server;
    char* set_cookie;
    char* trailer;
    char* transfer_encoding;
    char* upgrade;
    char* vary;
    char* via;
    char* warning;
    char* www_authenticate;
} response_headers_t;

typedef struct {
    request_line_t request_line;
    request_headers_t request_headers;
    status_line_t status_line;
    response_headers_t response_headers;
} http_message_t;

HParser* request_line_parser;
HParser* request_headers_parser;
HParser* status_line_parser;
HParser* response_headers_parser;
HParser* http_message_parser;

void init_parsers() {
    request_line_parser = h_sequence(
        h_string("GET"),
        h_string(" "),
        h_regex("[^ ]+"),
        h_string(" "),
        h_regex("HTTP/1\\.1"),
        h_eol,
        &(request_line_t){
            .method = h_capture(1),
            .request_uri = h_capture(2),
            .http_version = h_capture(3)
        }
    );

    request_headers_parser = h_sequence(
        h_optional(
            h_sequence(
                h_string("Accept: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(request_headers_t){
                    .accept = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Accept-Charset: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(request_headers_t){
                    .accept_charset = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Accept-Encoding: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(request_headers_t){
                    .accept_encoding = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Accept-Language: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(request_headers_t){
                    .accept_language = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Authorization: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(request_headers_t){
                    .authorization = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Cache-Control: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(request_headers_t){
                    .cache_control = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Connection: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(request_headers_t){
                    .connection = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Content-Length: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(request_headers_t){
                    .content_length = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Content-Type: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(request_headers_t){
                    .content_type = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Date: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(request_headers_t){
                    .date = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Expect: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(request_headers_t){
                    .expect = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("From: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(request_headers_t){
                    .from = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Host: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(request_headers_t){
                    .host = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("If-Match: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(request_headers_t){
                    .if_match = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("If-Modified-Since: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(request_headers_t){
                    .if_modified_since = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("If-None-Match: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(request_headers_t){
                    .if_none_match = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("If-Range: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(request_headers_t){
                    .if_range = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("If-Unmodified-Since: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(request_headers_t){
                    .if_unmodified_since = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Max-Forwards: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(request_headers_t){
                    .max_forwards = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Proxy-Authorization: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(request_headers_t){
                    .proxy_authorization = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Range: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(request_headers_t){
                    .range = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Referer: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(request_headers_t){
                    .referer = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("TE: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(request_headers_t){
                    .te = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("User-Agent: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(request_headers_t){
                    .user_agent = h_capture(1)
                }
            )
        ),
        h_eol
    );

    status_line_parser = h_sequence(
        h_string("HTTP/1.1 "),
        h_regex("\\d{3}"),
        h_string(" "),
        h_regex("[^\\r\\n]+"),
        h_eol,
        &(status_line_t){
            .http_version = h_capture(0),
            .status_code = atoi(h_capture(1)),
            .reason_phrase = h_capture(2)
        }
    );

    response_headers_parser = h_sequence(
        h_optional(
            h_sequence(
                h_string("Accept-Ranges: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(response_headers_t){
                    .accept_ranges = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Age: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(response_headers_t){
                    .age = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Allow: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(response_headers_t){
                    .allow = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Cache-Control: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(response_headers_t){
                    .cache_control = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Connection: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(response_headers_t){
                    .connection = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Content-Encoding: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(response_headers_t){
                    .content_encoding = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Content-Language: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(response_headers_t){
                    .content_language = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Content-Length: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(response_headers_t){
                    .content_length = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Content-Location: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(response_headers_t){
                    .content_location = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Content-MD5: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(response_headers_t){
                    .content_md5 = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Content-Range: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(response_headers_t){
                    .content_range = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Content-Type: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(response_headers_t){
                    .content_type = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Date: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(response_headers_t){
                    .date = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("ETag: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(response_headers_t){
                    .etag = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Expires: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(response_headers_t){
                    .expires = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Last-Modified: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(response_headers_t){
                    .last_modified = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Location: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(response_headers_t){
                    .location = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Proxy-Authenticate: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(response_headers_t){
                    .proxy_authenticate = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Retry-After: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(response_headers_t){
                    .retry_after = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Server: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(response_headers_t){
                    .server = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Set-Cookie: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(response_headers_t){
                    .set_cookie = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Trailer: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(response_headers_t){
                    .trailer = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Transfer-Encoding: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(response_headers_t){
                    .transfer_encoding = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Upgrade: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(response_headers_t){
                    .upgrade = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Vary: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(response_headers_t){
                    .vary = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Via: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(response_headers_t){
                    .via = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("Warning: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(response_headers_t){
                    .warning = h_capture(1)
                }
            )
        ),
        h_optional(
            h_sequence(
                h_string("WWW-Authenticate: "),
                h_regex("[^\\r\\n]+"),
                h_eol,
                &(response_headers_t){
                    .www_authenticate = h_capture(1)
                }
            )
        ),
        h_eol
    );

    http_message_parser = h_sequence(
        request_line_parser,
        request_headers_parser,
        status_line_parser,
        response_headers_parser,
        &(http_message_t){
            .request_line = h_capture(0),
            .request_headers = h_capture(1),
            .status_line = h_capture(2),
            .response_headers = h_capture(3)
        }
    );
}

int main(int argc, char** argv) {
    init_parsers();

    if (argc != 2) {
        printf("Usage: %s <file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        printf("Failed to open file %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        printf("Failed to allocate memory\n");
        fclose(file);
        return 1;
    }

    size_t bytes_read = fread(buffer, 1, file_size, file);
    if (bytes_read != file_size) {
        printf("Failed to read file %s\n", argv[1]);
        free(buffer);
        fclose(file);
        return 1;
    }

    fclose(file);

    http_message_t message;
    HParseResult* result = h_parse(http_message_parser, buffer, file_size);
    if (result->status != H_OK) {
        printf("Failed to parse HTTP message\n");
        free(buffer);
        return 1;
    }

    printf("Request Line:\n");
    printf("  Method: %s\n", message.request_line.method);
    printf("  Request URI: %s\n", message.request_line.request_uri);
    printf("  HTTP Version: %s\n", message.request_line.http_version);

    printf("Request Headers:\n");
    if (message.request_headers.accept) printf("  Accept: %s\n", message.request_headers.accept);
    if (message.request_headers.accept_charset) printf("  Accept-Charset: %s\n", message.request_headers.accept_charset);
    if (message.request_headers.accept_encoding) printf("  Accept-Encoding: %s\n", message.request_headers.accept_encoding);
    if (message.request_headers.accept_language) printf("  Accept-Language: %s\n", message.request_headers.accept_language);
    if (message.request_headers.authorization) printf("  Authorization: %s\n", message.request_headers.authorization);
    if (message.request_headers.cache_control) printf("  Cache-Control: %s\n", message.request_headers.cache_control);
    if (message.request_headers.connection) printf("  Connection: %s\n", message.request_headers.connection);
    if (message.request_headers.content_length) printf("  Content-Length: %s\n", message.request_headers.content_length);
    if (message.request_headers.content_type) printf("  Content