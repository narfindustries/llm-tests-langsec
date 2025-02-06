grammar HTTP;

import std.ascii;

type Version = struct {
    major u8 = ascii.digit;
    dot char = '.';
    minor u8 = ascii.digit;
};

type RequestLine = struct {
    method string = until(' ');
    space1 char = ' ';
    uri string = until(' ');
    space2 char = ' ';
    version Version;
    crlf u16 = 0x0D0A;
};

type StatusLine = struct {
    version Version;
    space char = ' ';
    statusCode u16 = ascii.digit * 3;
    space2 char = ' ';
    reasonPhrase string = until(0x0D0A);
    crlf u16 = 0x0D0A;
};

type Header = struct {
    name string = until(':');
    colon char = ':';
    space char = ' ';
    value string = until(0x0D0A);
    crlf u16 = 0x0D0A;
};

type Headers = struct {
    headers list(Header);
    crlf u16 = 0x0D0A;
};

type MessageBody = struct {
    content bytes;
};

type Request = struct {
    requestLine RequestLine;
    headers Headers;
    body MessageBody;
};

type Response = struct {
    statusLine StatusLine;
    headers Headers;
    body MessageBody;
};

type HTTPMessage = union {
    request Request;
    response Response;
};