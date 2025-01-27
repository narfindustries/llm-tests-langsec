// HTTP/1.1 Protocol Specification
module HTTP-1.1

// Primitive Types
type Byte = U8
type ASCII = U8 where (x) -> x >= 32 && x < 127
type ALPHA = ASCII where (x) -> (x >= 65 && x <= 90) || (x >= 97 && x <= 122)
type DIGIT = ASCII where (x) -> x >= 48 && x <= 57
type HEXDIG = DIGIT | 'A'..'F' | 'a'..'f'

// Core Character Sets
type CTL = U8 where (x) -> x < 32 || x == 127
type SP = U8 == 32
type HT = U8 == 9
type CRLF = [13, 10]
type LWS = (SP | HT)+
type TEXT = ASCII where (x) -> x != 127
type CHAR = ASCII

// Header Parsing
type HeaderName = ALPHA+
type HeaderValue = TEXT*
type Header = {
    name: HeaderName,
    value: HeaderValue
}

// Request Line Components
type Method = "GET" | "POST" | "PUT" | "DELETE" | "HEAD" | "OPTIONS" | "TRACE" | "CONNECT"
type RequestURI = ASCII+
type HTTPVersion = "HTTP/1.1"

// Request Structure
type RequestLine = {
    method: Method,
    uri: RequestURI,
    version: HTTPVersion,
    terminal: CRLF
}

// Message Structure
type HttpRequest = {
    request_line: RequestLine,
    headers: Header*,
    body: U8*
}

// Top-Level Parser
def parse_request(input: U8*) -> HttpRequest = {
    request_line = parse_request_line(input),
    headers = parse_headers(input[request_line.end..]),
    body = parse_body(input[headers.end..])
}

def parse_request_line(input: U8*) -> RequestLine = {
    method = parse_method(input),
    sp1 = parse_whitespace(input[method.end..]),
    uri = parse_uri(input[sp1.end..]),
    sp2 = parse_whitespace(input[uri.end..]),
    version = parse_version(input[sp2.end..]),
    terminal = parse_crlf(input[version.end..])
}

def parse_method(input: U8*) -> Method = {
    // Method parsing logic here
}

def parse_uri(input: U8*) -> RequestURI = {
    // URI parsing logic here
}

def parse_version(input: U8*) -> HTTPVersion = {
    // Version parsing logic here
}

def parse_headers(input: U8*) -> Header* = {
    // Header parsing logic here
}

def parse_body(input: U8*) -> U8* = {
    // Body parsing logic here
}

def parse_whitespace(input: U8*) -> SP = {
    // Whitespace parsing logic
}

def parse_crlf(input: U8*) -> CRLF = {
    // CRLF parsing logic
}