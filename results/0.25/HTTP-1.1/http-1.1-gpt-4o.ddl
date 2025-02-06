HTTPMessage = {
    start: RequestLine | StatusLine;
    headers: HeaderField[];
    empty_line: "\r\n";
    body: bytes;
}

RequestLine = {
    method: Token;
    SP: " ";
    request_uri: URI;
    SP: " ";
    http_version: HTTPVersion;
    CRLF: "\r\n";
}

StatusLine = {
    http_version: HTTPVersion;
    SP: " ";
    status_code: StatusCode;
    SP: " ";
    reason_phrase: ReasonPhrase;
    CRLF: "\r\n";
}

HTTPVersion = {
    "HTTP/";
    major: digit;
    ".";
    minor: digit;
}

HeaderField = {
    field_name: Token;
    ": ";
    field_value: FieldValue;
    CRLF: "\r\n";
}

FieldValue = {
    value: bytes;
}

Token = {
    token: bytes;
}

URI = {
    uri: bytes;
}

ReasonPhrase = {
    phrase: bytes;
}

StatusCode = {
    digit1: digit;
    digit2: digit;
    digit3: digit;
}

digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";