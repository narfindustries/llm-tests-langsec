#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define HAMMER_HEADER_ONLY

typedef enum {
    HTTP_METHOD_GET,
    HTTP_METHOD_HEAD,
    HTTP_METHOD_POST,
    HTTP_METHOD_PUT,
    HTTP_METHOD_DELETE,
    HTTP_METHOD_CONNECT,
    HTTP_METHOD_OPTIONS,
    HTTP_METHOD_TRACE
} http_method;

typedef enum {
    HTTP_VERSION_1_1
} http_version;

typedef struct {
    http_method method;
    char* request_uri;
    http_version version;
} http_request_line;

typedef struct {
    char* name;
    char* value;
} http_header;

typedef struct {
    http_request_line request_line;
    http_header* headers;
    int header_count;
    char* body;
} http_request;

typedef enum {
    HTTP_STATUS_CODE_100,
    HTTP_STATUS_CODE_101,
    HTTP_STATUS_CODE_200,
    HTTP_STATUS_CODE_201,
    HTTP_STATUS_CODE_202,
    HTTP_STATUS_CODE_203,
    HTTP_STATUS_CODE_204,
    HTTP_STATUS_CODE_205,
    HTTP_STATUS_CODE_206,
    HTTP_STATUS_CODE_300,
    HTTP_STATUS_CODE_301,
    HTTP_STATUS_CODE_302,
    HTTP_STATUS_CODE_303,
    HTTP_STATUS_CODE_304,
    HTTP_STATUS_CODE_305,
    HTTP_STATUS_CODE_307,
    HTTP_STATUS_CODE_400,
    HTTP_STATUS_CODE_401,
    HTTP_STATUS_CODE_402,
    HTTP_STATUS_CODE_403,
    HTTP_STATUS_CODE_404,
    HTTP_STATUS_CODE_405,
    HTTP_STATUS_CODE_406,
    HTTP_STATUS_CODE_407,
    HTTP_STATUS_CODE_408,
    HTTP_STATUS_CODE_409,
    HTTP_STATUS_CODE_410,
    HTTP_STATUS_CODE_411,
    HTTP_STATUS_CODE_412,
    HTTP_STATUS_CODE_413,
    HTTP_STATUS_CODE_414,
    HTTP_STATUS_CODE_415,
    HTTP_STATUS_CODE_416,
    HTTP_STATUS_CODE_417,
    HTTP_STATUS_CODE_500,
    HTTP_STATUS_CODE_501,
    HTTP_STATUS_CODE_502,
    HTTP_STATUS_CODE_503,
    HTTP_STATUS_CODE_504,
    HTTP_STATUS_CODE_505
} http_status_code;

typedef struct {
    http_version version;
    http_status_code status_code;
    char* reason_phrase;
} http_status_line;

typedef struct {
    http_status_line status_line;
    http_header* headers;
    int header_count;
    char* body;
} http_response;

void* http_method_parser(void* input) {
    char* data = (char*)input;
    if (strncmp(data, "GET", 3) == 0) {
        return (void*)(intptr_t)HTTP_METHOD_GET;
    } else if (strncmp(data, "HEAD", 4) == 0) {
        return (void*)(intptr_t)HTTP_METHOD_HEAD;
    } else if (strncmp(data, "POST", 4) == 0) {
        return (void*)(intptr_t)HTTP_METHOD_POST;
    } else if (strncmp(data, "PUT", 3) == 0) {
        return (void*)(intptr_t)HTTP_METHOD_PUT;
    } else if (strncmp(data, "DELETE", 6) == 0) {
        return (void*)(intptr_t)HTTP_METHOD_DELETE;
    } else if (strncmp(data, "CONNECT", 7) == 0) {
        return (void*)(intptr_t)HTTP_METHOD_CONNECT;
    } else if (strncmp(data, "OPTIONS", 7) == 0) {
        return (void*)(intptr_t)HTTP_METHOD_OPTIONS;
    } else if (strncmp(data, "TRACE", 5) == 0) {
        return (void*)(intptr_t)HTTP_METHOD_TRACE;
    } else {
        return NULL;
    }
}

void* http_version_parser(void* input) {
    char* data = (char*)input;
    if (strncmp(data, "HTTP/1.1", 8) == 0) {
        return (void*)(intptr_t)HTTP_VERSION_1_1;
    } else {
        return NULL;
    }
}

void* http_request_line_parser(void* input) {
    char* data = (char*)input;
    void* method = http_method_parser(data);
    if (method == NULL) {
        return NULL;
    }
    data += 3;
    if (*data != ' ') {
        return NULL;
    }
    data++;
    char* request_uri = data;
    while (*data != ' ') {
        data++;
    }
    *data = '\0';
    void* version = http_version_parser(data + 1);
    if (version == NULL) {
        return NULL;
    }
    http_request_line* request_line = malloc(sizeof(http_request_line));
    request_line->method = (http_method)(intptr_t)method;
    request_line->request_uri = request_uri;
    request_line->version = (http_version)(intptr_t)version;
    return request_line;
}

void* http_header_parser(void* input) {
    char* data = (char*)input;
    char* name = data;
    while (*data != ':') {
        data++;
    }
    *data = '\0';
    data++;
    while (*data == ' ') {
        data++;
    }
    char* value = data;
    while (*data != '\r') {
        data++;
    }
    *data = '\0';
    http_header* header = malloc(sizeof(http_header));
    header->name = name;
    header->value = value;
    return header;
}

void* http_request_parser(void* input) {
    char* data = (char*)input;
    void* request_line_ptr = http_request_line_parser(data);
    if (request_line_ptr == NULL) {
        return NULL;
    }
    http_request_line* request_line = (http_request_line*)request_line_ptr;
    data += strlen(request_line->request_uri) + strlen("HTTP/1.1") + 4;
    http_header* headers = NULL;
    int header_count = 0;
    while (*data != '\0') {
        void* header_ptr = http_header_parser(data);
        if (header_ptr == NULL) {
            break;
        }
        http_header* header = (http_header*)header_ptr;
        headers = realloc(headers, (header_count + 1) * sizeof(http_header));
        headers[header_count] = *header;
        header_count++;
        data += strlen(header->name) + strlen(header->value) + 2;
    }
    http_request* request = malloc(sizeof(http_request));
    request->request_line = *request_line;
    request->headers = headers;
    request->header_count = header_count;
    request->body = NULL;
    free(request_line);
    return request;
}

void* http_status_code_parser(void* input) {
    char* data = (char*)input;
    if (strncmp(data, "100", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_100;
    } else if (strncmp(data, "101", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_101;
    } else if (strncmp(data, "200", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_200;
    } else if (strncmp(data, "201", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_201;
    } else if (strncmp(data, "202", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_202;
    } else if (strncmp(data, "203", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_203;
    } else if (strncmp(data, "204", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_204;
    } else if (strncmp(data, "205", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_205;
    } else if (strncmp(data, "206", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_206;
    } else if (strncmp(data, "300", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_300;
    } else if (strncmp(data, "301", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_301;
    } else if (strncmp(data, "302", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_302;
    } else if (strncmp(data, "303", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_303;
    } else if (strncmp(data, "304", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_304;
    } else if (strncmp(data, "305", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_305;
    } else if (strncmp(data, "307", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_307;
    } else if (strncmp(data, "400", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_400;
    } else if (strncmp(data, "401", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_401;
    } else if (strncmp(data, "402", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_402;
    } else if (strncmp(data, "403", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_403;
    } else if (strncmp(data, "404", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_404;
    } else if (strncmp(data, "405", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_405;
    } else if (strncmp(data, "406", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_406;
    } else if (strncmp(data, "407", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_407;
    } else if (strncmp(data, "408", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_408;
    } else if (strncmp(data, "409", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_409;
    } else if (strncmp(data, "410", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_410;
    } else if (strncmp(data, "411", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_411;
    } else if (strncmp(data, "412", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_412;
    } else if (strncmp(data, "413", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_413;
    } else if (strncmp(data, "414", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_414;
    } else if (strncmp(data, "415", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_415;
    } else if (strncmp(data, "416", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_416;
    } else if (strncmp(data, "417", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_417;
    } else if (strncmp(data, "500", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_500;
    } else if (strncmp(data, "501", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_501;
    } else if (strncmp(data, "502", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_502;
    } else if (strncmp(data, "503", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_503;
    } else if (strncmp(data, "504", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_504;
    } else if (strncmp(data, "505", 3) == 0) {
        return (void*)(intptr_t)HTTP_STATUS_CODE_505;
    } else {
        return NULL;
    }
}

void* http_status_line_parser(void* input) {
    char* data = (char*)input;
    void* version = http_version_parser(data);
    if (version == NULL) {
        return NULL;
    }
    data += 8;
    if (*data != ' ') {
        return NULL;
    }
    data++;
    void* status_code = http_status_code_parser(data);
    if (status_code == NULL) {
        return NULL;
    }
    data += 3;
    if (*data != ' ') {
        return NULL;
    }
    data++;
    char* reason_phrase = data;
    while (*data != '\r') {
        data++;
    }
    *data = '\0';
    http_status_line* status_line = malloc(sizeof(http_status_line));
    status_line->version = (http_version)(intptr_t)version;
    status_line->status_code = (http_status_code)(intptr_t)status_code;
    status_line->reason_phrase = reason_phrase;
    return status_line;
}

void* http_response_parser(void* input) {
    char* data = (char*)input;
    void* status_line_ptr = http_status_line_parser(data);
    if (status_line_ptr == NULL) {
        return NULL;
    }
    http_status_line* status_line = (http_status_line*)status_line_ptr;
    data += strlen("HTTP/1.1") + 3 + 3 + strlen(status_line->reason_phrase) + 4;
    http_header* headers = NULL;
    int header_count = 0;
    while (*data != '\0') {
        void* header_ptr = http_header_parser(data);
        if (header_ptr == NULL) {
            break;
        }
        http_header* header = (http_header*)header_ptr;
        headers = realloc(headers, (header_count + 1) * sizeof(http_header));
        headers[header_count] = *header;
        header_count++;
        data += strlen(header->name) + strlen(header->value) + 2;
    }
    http_response* response = malloc(sizeof(http_response));
    response->status_line = *status_line;
    response->headers = headers;
    response->header_count = header_count;
    response->body = NULL;
    free(status_line);
    return response;
}

int main(int argc, char** argv) {
    if (argc != 2) {
        printf("Usage: %s <file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file: %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    char* data = malloc(file_size);
    if (!data) {
        printf("Error allocating memory\n");
        fclose(file);
        return 1;
    }

    size_t read_size = fread(data, 1, file_size, file);
    if (read_size != file_size) {
        printf("Error reading file\n");
        free(data);
        fclose(file);
        return 1;
    }

    fclose(file);

    void* request_ptr = http_request_parser(data);
    if (request_ptr != NULL) {
        http_request* request = (http_request*)request_ptr;
        printf("Parsed request:\n");
        printf("Method: %d\n", request->request_line.method);
        printf("Request URI: %s\n", request->request_line.request_uri);
        printf("Version: %d\n", request->request_line.version);
        for (int i = 0; i < request->header_count; i++) {
            printf("Header %d: %s = %s\n", i, request->headers[i].name, request->headers[i].value);
        }
        free(request->headers);
        free(request);
    } else {
        void* response_ptr = http_response_parser(data);
        if (response_ptr != NULL) {
            http_response* response = (http_response*)response_ptr;
            printf("Parsed response:\n");
            printf("Version: %d\n", response->status_line.version);
            printf("Status Code: %d\n", response->status_line.status_code);
            printf("Reason Phrase: %s\n", response->status_line.reason_phrase);
            for (int i = 0; i < response->header_count; i++) {
                printf("Header %d: %s = %s\n", i, response->headers[i].name, response->headers[i].value);
            }
            free(response->headers);
            free(response);
        } else {
            printf("Error parsing HTTP message\n");
        }
    }

    free(data);
    return 0;
}