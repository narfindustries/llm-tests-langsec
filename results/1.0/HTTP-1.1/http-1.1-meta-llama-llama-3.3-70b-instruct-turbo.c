#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Define the structure for HTTP request
typedef struct {
    char method[10];
    char url[100];
    char http_version[10];
    char headers[1000];
    char body[1000];
} http_request_t;

// Define the structure for HTTP response
typedef struct {
    char http_version[10];
    int status_code;
    char reason_phrase[100];
    char headers[1000];
    char body[1000];
} http_response_t;

// Function to parse HTTP request
http_request_t parse_http_request(char* data) {
    http_request_t request;
    sscanf(data, "%s %s %s", request.method, request.url, request.http_version);
    // Parse headers and body
    char* header_ptr = strstr(data, "\r\n");
    if (header_ptr) {
        strcpy(request.headers, header_ptr + 2);
        char* body_ptr = strstr(header_ptr + 2, "\r\n\r\n");
        if (body_ptr) {
            strcpy(request.body, body_ptr + 4);
        }
    }
    return request;
}

// Function to generate HTTP response
http_response_t generate_http_response(int status_code, char* reason_phrase) {
    http_response_t response;
    strcpy(response.http_version, "HTTP/1.1");
    response.status_code = status_code;
    strcpy(response.reason_phrase, reason_phrase);
    sprintf(response.headers, "Content-Type: text/html\r\n");
    sprintf(response.body, "<html><body>Status Code: %d</body></html>", status_code);
    return response;
}

// Function to handle HTTP request and send response
void handle_http_request(char* data) {
    http_request_t request = parse_http_request(data);
    int status_code = 200;
    char reason_phrase[100] = "OK";
    http_response_t response = generate_http_response(status_code, reason_phrase);
    // Send response back to client
    printf("HTTP/1.1 %d %s\r\n", response.status_code, response.reason_phrase);
    printf("%s\r\n", response.headers);
    printf("\r\n");
    printf("%s", response.body);
}

int main() {
    char data[1000];
    // Read data from socket or file
    // For demonstration purposes, we will use a static string
    char* request = "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n";
    strcpy(data, request);
    handle_http_request(data);
    return 0;
}