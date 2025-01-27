module HTTP_1_1

type HTTPMessage = struct {
    start_line: StartLine,
    headers: Headers,
    body: Body
}

type StartLine = struct {
    method: Method,
    uri: URI,
    version: Version
}

type Method = enum : uint8 {
    GET = 0x01,
    POST = 0x02,
    PUT = 0x03,
    DELETE = 0x04,
    HEAD = 0x05,
    OPTIONS = 0x06,
    TRACE = 0x07,
    CONNECT = 0x08
}

type URI = string

type Version = struct {
    major: uint8,
    minor: uint8
}

type Headers = list of Header

type Header = struct {
    name: string,
    value: string
}

type Body = struct {
    content: bytes
}