// Daedalus specification for HTTP/1.1

namespace http

// Define the HTTP version
type HttpVersion = "HTTP/1.1"

// Define the HTTP methods
enum HttpMethod : string {
    GET = "GET",
    POST = "POST",
    PUT = "PUT",
    DELETE = "DELETE",
    HEAD = "HEAD",
    OPTIONS = "OPTIONS",
    TRACE = "TRACE",
    CONNECT = "CONNECT"
}

// Define a request line
struct RequestLine {
    method: HttpMethod
    uri: string
    version: HttpVersion
    "\r\n"
}

// Define a status line
struct StatusLine {
    version: HttpVersion
    status_code: uint16
    reason_phrase: string
    "\r\n"
}

// Define a header field
struct HeaderField {
    name: string
    ": "
    value: string
    "\r\n"
}

// Define the HTTP headers
struct Headers {
    fields: HeaderField[]
    "\r\n"
}

// Define the HTTP request
struct HttpRequest {
    request_line: RequestLine
    headers: Headers
    body: bytes
}

// Define the HTTP response
struct HttpResponse {
    status_line: StatusLine
    headers: Headers
    body: bytes
}

// Define the top-level HTTP message
type HttpMessage = choice {
    1: HttpRequest
    2: HttpResponse
}
