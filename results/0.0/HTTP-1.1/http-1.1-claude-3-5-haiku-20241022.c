#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <hammer/hammer.h>

// HTTP Method Enum
typedef enum {
    HTTP_GET,
    HTTP_POST,
    HTTP_HEAD,
    HTTP_PUT,
    HTTP_DELETE,
    HTTP_TRACE,
    HTTP_OPTIONS,
    HTTP_CONNECT
} HttpMethod;

// HTTP Version Struct
typedef struct {
    int major;
    int minor;
} HttpVersion;

// Header Struct
typedef struct {
    char* name;
    char* value;
} HttpHeader;

// Request Struct
typedef struct {
    HttpMethod method;
    char* uri;
    HttpVersion version;
    HttpHeader* headers;
    int header_count;
    char* body;
} HttpRequest;

// Response Struct
typedef struct {
    HttpVersion version;
    int status_code;
    char* status_message;
    HttpHeader* headers;
    int header_count;
    char* body;
} HttpResponse;

// Parser Combinators
HParser* http_method_parser() {
    return h_choice(
        h_token("GET", 3),
        h_token("POST", 4),
        h_token("HEAD", 4),
        h_token("PUT", 3),
        h_token("DELETE", 6),
        h_token("TRACE", 5),
        h_token("OPTIONS", 7),
        h_token("CONNECT", 7),
        NULL
    );
}

HParser* http_version_parser() {
    return h_sequence(
        h_token("HTTP/", 5),
        h_int_range(h_ch('0'), h_ch('9')),
        h_ch('.'),
        h_int_range(h_ch('0'), h_ch('9')),
        NULL
    );
}

HParser* header_parser() {
    return h_sequence(
        h_many1(h_not(h_ch(':'))),  // Header name
        h_token(": ", 2),
        h_many1(h_not(h_ch('\r'))), // Header value
        h_token("\r\n", 2),
        NULL
    );
}

HParser* request_parser() {
    return h_sequence(
        http_method_parser(),
        h_ch(' '),
        h_many1(h_not(h_ch(' '))),  // URI
        h_ch(' '),
        http_version_parser(),
        h_token("\r\n", 2),
        h_many(header_parser()),
        h_token("\r\n", 2),
        h_optional(h_many(h_ch('\0'))), // Optional body
        NULL
    );
}

HParser* response_parser() {
    return h_sequence(
        http_version_parser(),
        h_ch(' '),
        h_int_range(h_ch('1'), h_ch('5')),
        h_int_range(h_ch('0'), h_ch('9')),
        h_int_range(h_ch('0'), h_ch('9')),
        h_ch(' '),
        h_many1(h_not(h_ch('\r'))), // Status Message
        h_token("\r\n", 2),
        h_many(header_parser()),
        h_token("\r\n", 2),
        h_optional(h_many(h_ch('\0'))), // Optional body
        NULL
    );
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <http_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
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

    HParser* parser = h_choice(request_parser(), response_parser(), NULL);
    HParseResult* result = h_parse(parser, buffer, file_size);

    if (result && result->ast) {
        printf("Parsing successful\n");
    } else {
        printf("Parsing failed\n");
    }

    free(buffer);
    h_parse_result_free(result);
    return 0;
}