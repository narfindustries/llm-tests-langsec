#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Structure to represent a HTTP header
typedef struct {
    char *name;
    char *value;
} HTTPHeader;

// Structure to represent a HTTP request
typedef struct {
    char *method;
    char *path;
    char *version;
    HTTPHeader *headers;
    int numHeaders;
    char *body;
} HTTPRequest;


// Function to parse a HTTP request (simplified for demonstration)
HTTPRequest* parseHTTPRequest(const char *request) {
    HTTPRequest *req = (HTTPRequest*)malloc(sizeof(HTTPRequest));
    if (req == NULL) return NULL;

    // Basic parsing (replace with a robust parser for production)
    char *line = strtok((char*)request, "\r\n");
    char *token;

    token = strtok(line, " ");
    req->method = strdup(token);
    token = strtok(NULL, " ");
    req->path = strdup(token);
    token = strtok(NULL, " ");
    req->version = strdup(token);

    req->headers = NULL;
    req->numHeaders = 0;
    req->body = NULL;


    line = strtok(NULL, "\r\n\r\n");
    while (line != NULL && strcmp(line, "") != 0) {
        req->numHeaders++;
        req->headers = (HTTPHeader*)realloc(req->headers, req->numHeaders * sizeof(HTTPHeader));
        if (req->headers == NULL) {
            free(req);
            return NULL;
        }
        token = strtok(line, ":");
        req->headers[req->numHeaders -1].name = strdup(token);
        token = strtok(NULL, "\r\n");
        req->headers[req->numHeaders - 1].value = strdup(token);
        line = strtok(NULL, "\r\n\r\n");
    }

    //Body handling (simplified)
    if (line != NULL) {
        req->body = strdup(line);
    }

    return req;
}


// Function to free the allocated memory for a HTTP request
void freeHTTPRequest(HTTPRequest *req) {
    if (req == NULL) return;
    free(req->method);
    free(req->path);
    free(req->version);
    for (int i = 0; i < req->numHeaders; i++) {
        free(req->headers[i].name);
        free(req->headers[i].value);
    }
    free(req->headers);
    free(req->body);
    free(req);
}


int main() {
    char request[] = "GET /index.html HTTP/1.1\r\nHost: example.com\r\nConnection: close\r\n\r\n";

    HTTPRequest *httpRequest = parseHTTPRequest(request);

    if (httpRequest != NULL) {
        printf("Method: %s\n", httpRequest->method);
        printf("Path: %s\n", httpRequest->path);
        printf("Version: %s\n", httpRequest->version);
        for (int i = 0; i < httpRequest->numHeaders; i++) {
            printf("Header %d: %s: %s\n", i + 1, httpRequest->headers[i].name, httpRequest->headers[i].value);
        }
        if (httpRequest->body != NULL) {
            printf("Body: %s\n", httpRequest->body);
        }
        freeHTTPRequest(httpRequest);
    } else {
        fprintf(stderr, "Error parsing HTTP request.\n");
    }

    return 0;
}
