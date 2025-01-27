domain http {
  import byteorder;

  type RequestLine = struct {
    method: string(1..16, utf8);
    space1: byte = 32;
    requestUri: string(1..2048, utf8);
    space2: byte = 32;
    httpVersion: string(8, utf8);
    crlf: bytes = [13, 10];
  };

  type StatusLine = struct {
    httpVersion: string(8, utf8);
    space: byte = 32;
    statusCode: uint16;
    space2: byte = 32;
    reasonPhrase: string(1..64, utf8);
    crlf: bytes = [13, 10];
  };

  type Header = struct {
    name: string(1..32, utf8);
    colon: byte = 58;
    space: byte = 32;
    value: string(1..1024, utf8);
    crlf: bytes = [13, 10];
  };

  type Headers = array(Header, 1..16);

  type Message = struct {
    isRequest: bool;
    if isRequest then {
      requestLine: RequestLine;
    } else {
      statusLine: StatusLine;
    };
    headers: Headers;
    crlf: bytes = [13, 10];
    body: bytes;
  };

  grammar top = Message;
}