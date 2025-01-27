#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Structure to represent HTTP headers
typedef struct {
    char *name;
    char *value;
} HTTPHeader;

// Structure to represent an HTTP request
typedef struct {
    char *method;
    char *path;
    char *version;
    HTTPHeader *headers;
    int numHeaders;
    char *body;
} HTTPRequest;


// Function to parse an HTTP request (simplified for demonstration)
HTTPRequest* parseHTTPRequest(const char *request) {
    HTTPRequest *req = (HTTPRequest*)malloc(sizeof(HTTPRequest));
    if (req == NULL) return NULL;

    // Basic parsing (replace with more robust parsing in a real application)
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

    line = strtok(NULL, "\r\n\r\n"); //read until headers end

    while(line != NULL && strcmp(line,"") != 0){
        token = strtok(line,":");
        if(token != NULL){
            char *header_name = strdup(token);
            token = strtok(NULL, "\r\n");
            char *header_value = strdup(token);
            HTTPHeader *newHeader = (HTTPHeader*)malloc(sizeof(HTTPHeader));
            newHeader->name = header_name;
            newHeader->value = header_value;

            req->headers = realloc(req->headers, (req->numHeaders + 1) * sizeof(HTTPHeader));
            req->headers[req->numHeaders] = *newHeader;
            req->numHeaders++;
        }
        line = strtok(NULL, "\r\n\r\n");
    }


    //body
    if(line != NULL){
        req->body = strdup(line);
    }

    return req;
}


// Function to simulate the "hammer" functionality (replace with actual hammer logic)
void hammer(HTTPRequest *req) {
    printf("Hammering request:\n");
    printf("Method: %s\n", req->method);
    printf("Path: %s\n", req->path);
    printf("Version: %s\n", req->version);
    for (int i = 0; i < req->numHeaders; i++) {
        printf("Header: %s: %s\n", req->headers[i].name, req->headers[i].value);
    }
    if(req->body != NULL) printf("Body: %s\n", req->body);
    printf("\n");

    //Free memory
    free(req->method);
    free(req->path);
    free(req->version);
    for(int i = 0; i < req->numHeaders; i++){
        free(req->headers[i].name);
        free(req->headers[i].value);
    }
    free(req->headers);
    free(req->body);
    free(req);
}


int main() {
    char request[] = "GET /index.html HTTP/1.1\r\nHost: example.com\r\nUser-Agent: Hammer Client\r\nConnection: close\r\n\r\n";
    HTTPRequest *req = parseHTTPRequest(request);
    if(req != NULL) hammer(req);
    else printf("Error parsing request.\n");
    return 0;
}
