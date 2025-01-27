#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

// Define the structure for HTTP-1.1 meta data
typedef struct {
    uint8_t major_version;
    uint8_t minor_version;
} http_1_1_meta_t;

// Define the structure for HTTP-1.1 request data
typedef struct {
    char method[10];
    char uri[100];
    http_1_1_meta_t meta;
} http_1_1_request_t;

// Define the structure for HTTP-1.1 response data
typedef struct {
    http_1_1_meta_t meta;
    uint16_t status_code;
    char reason_phrase[50];
} http_1_1_response_t;

// Define the structure for HTTP-1.1 message data
typedef struct {
    http_1_1_request_t request;
    http_1_1_response_t response;
} http_1_1_message_t;

// Define the function to parse HTTP-1.1 message data
http_1_1_message_t* parse_http_1_1_message(const char* data, size_t length) {
    http_1_1_message_t* message = (http_1_1_message_t*)malloc(sizeof(http_1_1_message_t));
    if (message == NULL) {
        return NULL;
    }

    // Parse the request line
    const char* ptr = data;
    size_t len = strlen(ptr);
    if (len < 10) {
        free(message);
        return NULL;
    }
    strcpy(message->request.method, ptr);
    ptr += strlen(ptr) + 1;
    len -= strlen(ptr) + 1;
    if (len < 100) {
        free(message);
        return NULL;
    }
    strcpy(message->request.uri, ptr);
    ptr += strlen(ptr) + 1;
    len -= strlen(ptr) + 1;

    // Parse the HTTP-1.1 meta data
    if (len < 2) {
        free(message);
        return NULL;
    }
    message->request.meta.major_version = *(ptr++);
    message->request.meta.minor_version = *(ptr++);

    // Parse the response line
    len -= 2;
    if (len < 10) {
        free(message);
        return NULL;
    }
    message->response.meta.major_version = message->request.meta.major_version;
    message->response.meta.minor_version = message->request.meta.minor_version;
    message->response.status_code = htons(200);
    strcpy(message->response.reason_phrase, "OK");

    return message;
}

int main() {
    // Create a sample HTTP-1.1 message
    char data[] = "GET /index.html HTTP/1.1\r\nHost: example.com\r\n\r\nHTTP/1.1 200 OK\r\n\r\n";
    size_t length = strlen(data);

    // Parse the HTTP-1.1 message
    http_1_1_message_t* message = parse_http_1_1_message(data, length);
    if (message == NULL) {
        printf("Failed to parse HTTP-1.1 message\n");
        return 1;
    }

    // Print the parsed message
    printf("Request Method: %s\n", message->request.method);
    printf("Request URI: %s\n", message->request.uri);
    printf("HTTP Version: %d.%d\n", message->request.meta.major_version, message->request.meta.minor_version);
    printf("Status Code: %d\n", ntohs(message->response.status_code));
    printf("Reason Phrase: %s\n", message->response.reason_phrase);

    // Free the memory allocated for the message
    free(message);

    return 0;
}