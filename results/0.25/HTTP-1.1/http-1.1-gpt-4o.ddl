module HTTP_1_1

type HTTPMessage = struct {
    start_line: StartLine,
    headers: Headers,
    empty_line: EmptyLine,
    body: Body
}

type StartLine = struct {
    method: Method,
    space1: Space,
    uri: URI,
    space2: Space,
    version: Version,
    crlf: CRLF
}

type Method = enum : string {
    GET = "GET",
    POST = "POST",
    PUT = "PUT",
    DELETE = "DELETE",
    HEAD = "HEAD",
    OPTIONS = "OPTIONS",
    TRACE = "TRACE",
    CONNECT = "CONNECT"
}

type URI = string

type Version = string

type Headers = list of Header

type Header = struct {
    name: HeaderName,
    colon: Colon,
    space: Space,
    value: HeaderValue,
    crlf: CRLF
}

type HeaderName = string

type Colon = ":"

type HeaderValue = string

type Space = " "

type CRLF = "\r\n"

type EmptyLine = CRLF

type Body = bytes