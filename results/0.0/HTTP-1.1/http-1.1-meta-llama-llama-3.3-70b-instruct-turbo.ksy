meta:
  id: http_1_1
  title: HTTP/1.1 protocol
  file-extension: http
  endian: le
  encoding: ascii

seq:
  - id: request_line
    type: request_line
  - id: headers
    type: headers
    repeat: expr
  - id: body
    type: body
    if: expr

types:
  request_line:
    seq:
      - id: method
        type: str
        len: 3
        enum: [GET, HEAD, POST, PUT, DELETE, CONNECT, OPTIONS, TRACE]
      - id: sp1
        type: str
        len: 1
        contents: ' '
      - id: request_uri
        type: str
      - id: sp2
        type: str
        len: 1
        contents: ' '
      - id: http_version
        type: str
        contents: 'HTTP/1.1'
      - id: crlf
        type: str
        len: 2
        contents: '\r\n'

  headers:
    seq:
      - id: header_name
        type: str
      - id: colon
        type: str
        len: 1
        contents: ':'
      - id: sp
        type: str
        len: 1
        contents: ' '
      - id: header_value
        type: str
      - id: crlf
        type: str
        len: 2
        contents: '\r\n'
    types:
      header_name:
        enum: [Accept, Accept-Charset, Accept-Encoding, Accept-Language, Authorization, Cache-Control, Connection, Content-Encoding, Content-Language, Content-Length, Content-Type, Date, Expect, From, Host, If-Match, If-Modified-Since, If-None-Match, If-Range, If-Unmodified-Since, Max-Forwards, Proxy-Authorization, Range, Referer, TE, Upgrade, User-Agent, Via, Warning]
      header_value:
        switch-on: header_name
        cases:
          Accept: str
          Accept-Charset: str
          Accept-Encoding: str
          Accept-Language: str
          Authorization: str
          Cache-Control: str
          Connection: str
          Content-Encoding: str
          Content-Language: str
          Content-Length: uint
          Content-Type: str
          Date: str
          Expect: str
          From: str
          Host: str
          If-Match: str
          If-Modified-Since: str
          If-None-Match: str
          If-Range: str
          If-Unmodified-Since: str
          Max-Forwards: uint
          Proxy-Authorization: str
          Range: str
          Referer: str
          TE: str
          Upgrade: str
          User-Agent: str
          Via: str
          Warning: str

  body:
    seq:
      - id: content
        type: str
        len: content_length

  response:
    seq:
      - id: http_version
        type: str
        contents: 'HTTP/1.1'
      - id: sp
        type: str
        len: 1
        contents: ' '
      - id: status_code
        type: uint
        enum: [100, 101, 200, 201, 202, 203, 204, 205, 206, 300, 301, 302, 303, 304, 305, 307, 400, 401, 402, 403, 404, 405, 406, 407, 408, 409, 410, 411, 412, 413, 414, 415, 416, 417, 500, 501, 502, 503, 504, 505]
      - id: sp
        type: str
        len: 1
        contents: ' '
      - id: reason_phrase
        type: str
      - id: crlf
        type: str
        len: 2
        contents: '\r\n'
      - id: headers
        type: headers
        repeat: expr
      - id: body
        type: body
        if: expr

instances:
  expr:
    seq:
      - id: repeat_expr
        type: bool
        if: _io.pos < _io.size

  content_length:
    seq:
      - id: content_length_value
        type: uint
        if: headers.header_name == 'Content-Length'