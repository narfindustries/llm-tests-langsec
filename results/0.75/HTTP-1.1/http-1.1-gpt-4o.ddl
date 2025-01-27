namespace HTTP-1.1

struct HttpRequest
{
    method: HttpMethod
    path: Path
    version: HttpVersion
    headers: HttpHeaders
    body: optional(HttpBody)
}

enum HttpMethod : u8
{
    GET = 'G',
    POST = 'P',
    PUT = 'U',
    DELETE = 'D',
    HEAD = 'H',
    OPTIONS = 'O',
    TRACE = 'T',
    CONNECT = 'C'
}

type Path : string
{
    encoding: utf8
    terminator: u8 = 0x20 // Space character
}

struct HttpVersion
{
    prefix: string = "HTTP/"
    major: u8
    separator: string = "."
    minor: u8
}

struct HttpHeaders
{
    headers: list(HttpHeader)
}

struct HttpHeader
{
    name: HeaderName
    separator: string = ": "
    value: HeaderValue
    terminator: string = "\r\n"
}

type HeaderName : string
{
    encoding: ascii
}

type HeaderValue : string
{
    encoding: utf8
}

struct HttpBody
{
    content: bytes
}

root
{
    request: HttpRequest
    terminator: string = "\r\n\r\n"
}