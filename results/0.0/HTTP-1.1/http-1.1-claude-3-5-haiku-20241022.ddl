module HTTP-1.1

type Request = {
    method: Method,
    uri: URI,
    version: Version,
    headers: list Header,
    body: optional bytes
}

type Response = {
    version: Version,
    status_code: StatusCode,
    status_text: string,
    headers: list Header,
    body: optional bytes
}

type Method = 
    | GET
    | POST
    | PUT
    | DELETE
    | HEAD
    | OPTIONS
    | PATCH

type URI = string

type Version = {
    major: uint8,
    minor: uint8
}

type Header = {
    name: string,
    value: string
}

type StatusCode = uint16

parser request: Request = 
    method <- parse_method,
    _ <- parse_whitespace,
    uri <- parse_uri,
    _ <- parse_whitespace,
    version <- parse_version,
    _ <- parse_crlf,
    headers <- parse_headers,
    body <- optional(parse_body),
    return {
        method = method,
        uri = uri, 
        version = version,
        headers = headers,
        body = body
    }

parser parse_method: Method =
    | "GET" -> GET
    | "POST" -> POST
    | "PUT" -> PUT
    | "DELETE" -> DELETE
    | "HEAD" -> HEAD
    | "OPTIONS" -> OPTIONS
    | "PATCH" -> PATCH

parser parse_uri: URI =
    uri <- take_while(lambda c. c != ' '),
    return uri

parser parse_version: Version =
    "HTTP/",
    major <- parse_digit,
    ".",
    minor <- parse_digit,
    return {
        major = major,
        minor = minor
    }

parser parse_digit: uint8 =
    c <- any_char,
    match c {
        '0' -> 0,
        '1' -> 1,
        '2' -> 2,
        '3' -> 3,
        '4' -> 4,
        '5' -> 5,
        '6' -> 6,
        '7' -> 7,
        '8' -> 8,
        '9' -> 9
    }

parser parse_headers: list Header =
    headers <- many(parse_header),
    return headers

parser parse_header: Header =
    name <- take_while(lambda c. c != ':'),
    ":",
    _ <- parse_whitespace,
    value <- take_while(lambda c. c != '\r'),
    _ <- parse_crlf,
    return {
        name = name,
        value = value
    }

parser parse_body: bytes =
    body <- take_while(lambda c. true),
    return body

parser parse_whitespace: unit =
    many(lambda c. c == ' ' || c == '\t')

parser parse_crlf: unit =
    "\r\n"