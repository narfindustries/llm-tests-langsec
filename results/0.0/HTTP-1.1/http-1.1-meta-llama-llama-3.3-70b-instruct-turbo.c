#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Define the HTTP request structure
typedef struct {
    char method[10];
    char path[256];
    char http_version[10];
    char headers[512];
    char body[1024];
} http_request_t;

// Define the HTTP response structure
typedef struct {
    char http_version[10];
    int status_code;
    char reason_phrase[256];
    char headers[512];
    char body[1024];
} http_response_t;

// Function to parse HTTP request
http_request_t* parse_http_request(char* request) {
    http_request_t* req = (http_request_t*) malloc(sizeof(http_request_t));
    // Parse the request line
    sscanf(request, "%s %s %s", req->method, req->path, req->http_version);
    // Parse the headers
    char* header_ptr = strstr(request, "\r\n");
    if (header_ptr != NULL) {
        strcpy(req->headers, header_ptr + 2);
    }
    // Parse the body
    char* body_ptr = strstr(request, "\r\n\r\n");
    if (body_ptr != NULL) {
        strcpy(req->body, body_ptr + 4);
    }
    return req;
}

// Function to generate HTTP response
http_response_t* generate_http_response(http_request_t* request) {
    http_response_t* resp = (http_response_t*) malloc(sizeof(http_response_t));
    // Generate the response line
    strcpy(resp->http_version, "HTTP/1.1");
    resp->status_code = 200;
    strcpy(resp->reason_phrase, "OK");
    // Generate the headers
    strcpy(resp->headers, "Content-Type: text/html\r\n");
    // Generate the body
    strcpy(resp->body, "<html><body>Hello World!</body></html>");
    return resp;
}

// Function to handle HTTP request
void handle_http_request(char* request) {
    http_request_t* req = parse_http_request(request);
    http_response_t* resp = generate_http_response(req);
    // Print the response
    printf("%s %d %s\r\n", resp->http_version, resp->status_code, resp->reason_phrase);
    printf("%s\r\n", resp->headers);
    printf("%s\r\n", resp->body);
    free(req);
    free(resp);
}

int main() {
    char request[] = "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n";
    handle_http_request(request);
    return 0;
}