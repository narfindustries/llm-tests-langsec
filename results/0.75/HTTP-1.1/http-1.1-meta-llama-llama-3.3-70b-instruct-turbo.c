#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Define the HTTP request structure
typedef struct {
    char* method;
    char* url;
    char* http_version;
    char** headers;
    int num_headers;
    char* body;
} http_request_t;

// Define the HTTP response structure
typedef struct {
    char* http_version;
    int status_code;
    char* reason_phrase;
    char** headers;
    int num_headers;
    char* body;
} http_response_t;

// Function to parse an HTTP request
http_request_t* parse_http_request(char* buffer, int length) {
    http_request_t* request = malloc(sizeof(http_request_t));
    request->method = NULL;
    request->url = NULL;
    request->http_version = NULL;
    request->headers = NULL;
    request->num_headers = 0;
    request->body = NULL;

    // Parse the request line
    char* request_line = strtok(buffer, "\r\n");
    if (request_line == NULL) {
        free(request);
        return NULL;
    }

    // Extract the method, URL, and HTTP version
    char* method = strtok(request_line, " ");
    if (method == NULL) {
        free(request);
        return NULL;
    }
    request->method = strdup(method);

    char* url = strtok(NULL, " ");
    if (url == NULL) {
        free(request);
        return NULL;
    }
    request->url = strdup(url);

    char* http_version = strtok(NULL, " ");
    if (http_version == NULL) {
        free(request);
        return NULL;
    }
    request->http_version = strdup(http_version);

    // Parse the headers
    char* header_line = strtok(NULL, "\r\n");
    while (header_line != NULL) {
        // Extract the header name and value
        char* header_name = strtok(header_line, ":");
        if (header_name == NULL) {
            break;
        }

        char* header_value = strtok(NULL, "\r\n");
        if (header_value == NULL) {
            break;
        }

        // Add the header to the request
        request->num_headers++;
        request->headers = realloc(request->headers, request->num_headers * sizeof(char*));
        request->headers[request->num_headers - 1] = malloc(strlen(header_name) + strlen(header_value) + 2);
        sprintf(request->headers[request->num_headers - 1], "%s: %s", header_name, header_value);

        header_line = strtok(NULL, "\r\n");
    }

    // Parse the body
    char* body = strtok(NULL, "\r\n\r\n");
    if (body != NULL) {
        request->body = strdup(body);
    }

    return request;
}

// Function to generate an HTTP response
http_response_t* generate_http_response(int status_code, char* reason_phrase, char** headers, int num_headers, char* body) {
    http_response_t* response = malloc(sizeof(http_response_t));
    response->http_version = NULL;
    response->status_code = status_code;
    response->reason_phrase = NULL;
    response->headers = NULL;
    response->num_headers = 0;
    response->body = NULL;

    // Set the HTTP version
    response->http_version = strdup("HTTP/1.1");

    // Set the reason phrase
    response->reason_phrase = strdup(reason_phrase);

    // Set the headers
    response->num_headers = num_headers;
    response->headers = headers;

    // Set the body
    response->body = strdup(body);

    return response;
}

int main() {
    // Example usage
    char* request_buffer = "GET /path/to/resource HTTP/1.1\r\nHost: example.com\r\nAccept: */*\r\n\r\n";
    http_request_t* request = parse_http_request(request_buffer, strlen(request_buffer));

    if (request != NULL) {
        // Process the request
        char* response_body = "Hello, World!";
        char** response_headers = malloc(2 * sizeof(char*));
        response_headers[0] = strdup("Content-Type: text/plain");
        response_headers[1] = strdup("Content-Length: 13");
        http_response_t* response = generate_http_response(200, "OK", response_headers, 2, response_body);

        // Print the response
        printf("%s %d %s\r\n", response->http_version, response->status_code, response->reason_phrase);
        for (int i = 0; i < response->num_headers; i++) {
            printf("%s\r\n", response->headers[i]);
        }
        printf("\r\n%s\r\n", response->body);

        // Free the response
        free(response->http_version);
        free(response->reason_phrase);
        for (int i = 0; i < response->num_headers; i++) {
            free(response->headers[i]);
        }
        free(response->headers);
        free(response->body);
        free(response);

        // Free the request
        free(request->method);
        free(request->url);
        free(request->http_version);
        for (int i = 0; i < request->num_headers; i++) {
            free(request->headers[i]);
        }
        free(request->headers);
        free(request->body);
        free(request);
    }

    return 0;
}