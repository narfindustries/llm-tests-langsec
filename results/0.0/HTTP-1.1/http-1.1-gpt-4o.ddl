module HTTPMessage {
    HTTPMessage = {
        start: RequestLine | StatusLine,
        headers: HeaderField*,
        emptyLine: "\r\n",
        body: Body?
    };

    RequestLine = {
        method: Token,
        space1: " ",
        requestURI: URI,
        space2: " ",
        httpVersion: HTTPVersion,
        crlf: "\r\n"
    };

    StatusLine = {
        httpVersion: HTTPVersion,
        space1: " ",
        statusCode: StatusCode,
        space2: " ",
        reasonPhrase: ReasonPhrase,
        crlf: "\r\n"
    };

    HeaderField = {
        fieldName: Token,
        colon: ":",
        space: " ",
        fieldValue: FieldValue,
        crlf: "\r\n"
    };

    Body = {
        content: Byte*
    };

    HTTPVersion = {
        prefix: "HTTP/",
        major: Digit,
        dot: ".",
        minor: Digit
    };

    StatusCode = {
        code: Digit Digit Digit
    };

    ReasonPhrase = {
        text: (Char - "\r\n")*
    };

    FieldValue = {
        value: (Char - "\r\n")*
    };

    Token = {
        text: (Char - " \t\r\n:")*
    };

    URI = {
        text: (Char - " \t\r\n")*
    };

    Char = {
        byte: Byte
    };

    Digit = {
        byte: "0".."9"
    };

    Byte = {
        byte: 0x00..0xFF
    };
}