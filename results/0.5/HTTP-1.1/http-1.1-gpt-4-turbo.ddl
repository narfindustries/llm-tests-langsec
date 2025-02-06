module HTTP;

import std.ascii;

type Version = struct {
    major u16 : Decimal;
    dot   char : '.';
    minor u16 : Decimal;
};

type RequestLine = struct {
    method  Token;
    space1  char : ' ';
    uri     Token;
    space2  char : ' ';
    version Version;
    crlf    char : '\r\n';
};

type StatusLine = struct {
    version Version;
    space1  char : ' ';
    status  u16 : Decimal;
    space2  char : ' ';
    reason  Token;
    crlf    char : '\r\n';
};

type Header = struct {
    name  Token;
    colon char : ':';
    space char : ' ';
    value Token;
    crlf  char : '\r\n';
};

type Headers = list(Header) until lookahead('\r\n');

type MessageBody = [u8];

type Request = struct {
    requestLine RequestLine;
    headers     Headers;
    crlf        char : '\r\n';
    body        MessageBody;
};

type Response = struct {
    statusLine StatusLine;
    headers    Headers;
    crlf       char : '\r\n';
    body       MessageBody;
};

type Token = while (ascii.isTokenChar) -> [u8];

type HTTP = union {
    request  Request;
    response Response;
};