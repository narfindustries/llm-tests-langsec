meta:
  id: http_1_1
  title: HTTP 1.1 protocol
  file-extension: http
  endian: le
  encoding: ascii

seq:
  - id: request_line
    type: request_line
  - id: headers
    type: headers
    repeat: expr
    repeat-until: crlf_crlf
  - id: body
    type: body
    if: has_body

types:
  request_line:
    seq:
      - id: method
        type: str
        len: 3
        enum: ["GET", "HEAD", "POST", "PUT", "DELETE", "CONNECT", "OPTIONS", "TRACE"]
      - id: sp1
        type: str
        len: 1
        enum: [" "]
      - id: request_uri
        type: str
      - id: sp2
        type: str
        len: 1
        enum: [" "]
      - id: http_version
        type: str
        enum: ["HTTP/1.1", "HTTP/1.0"]
      - id: crlf
        type: str
        len: 2
        enum: ["\r\n"]

  headers:
    seq:
      - id: field_name
        type: str
      - id: colon
        type: str
        len: 1
        enum: [":"]
      - id: sp
        type: str
        len: 1
        enum: [" "]
      - id: field_value
        type: str
      - id: crlf
        type: str
        len: 2
        enum: ["\r\n"]
    types:
      field_name:
        enum: ["Accept", "Accept-Charset", "Accept-Encoding", "Accept-Language", "Authorization", "Cache-Control", "Connection", "Content-Length", "Content-Type", "Date", "Expect", "From", "Host", "If-Match", "If-Modified-Since", "If-None-Match", "If-Range", "If-Unmodified-Since", "Max-Forwards", "Proxy-Authorization", "Range", "Referer", "TE", "Trailer", "Transfer-Encoding", "Upgrade", "User-Agent", "Via", "Warning"]
      field_value:
        switch-on: field_name
        cases:
          "Accept": str
          "Accept-Charset": str
          "Accept-Encoding": str
          "Accept-Language": str
          "Authorization": str
          "Cache-Control": str
          "Connection": str
          "Content-Length": uint
          "Content-Type": str
          "Date": str
          "Expect": str
          "From": str
          "Host": str
          "If-Match": str
          "If-Modified-Since": str
          "If-None-Match": str
          "If-Range": str
          "If-Unmodified-Since": str
          "Max-Forwards": uint
          "Proxy-Authorization": str
          "Range": str
          "Referer": str
          "TE": str
          "Trailer": str
          "Transfer-Encoding": str
          "Upgrade": str
          "User-Agent": str
          "Via": str
          "Warning": str

  crlf_crlf:
    seq:
      - id: crlf1
        type: str
        len: 2
        enum: ["\r\n"]
      - id: crlf2
        type: str
        len: 2
        enum: ["\r\n"]

  body:
    seq:
      - id: content
        type: str
        len: content_length

  response:
    seq:
      - id: status_line
        type: status_line
      - id: headers
        type: headers
        repeat: expr
        repeat-until: crlf_crlf
      - id: body
        type: body
        if: has_body

  status_line:
    seq:
      - id: http_version
        type: str
        enum: ["HTTP/1.1", "HTTP/1.0"]
      - id: sp1
        type: str
        len: 1
        enum: [" "]
      - id: status_code
        type: uint
        enum: [100, 101, 200, 201, 202, 203, 204, 205, 206, 300, 301, 302, 303, 304, 305, 307, 400, 401, 402, 403, 404, 405, 406, 407, 408, 409, 410, 411, 412, 413, 414, 415, 416, 417, 500, 501, 502, 503, 504, 505]
      - id: sp2
        type: str
        len: 1
        enum: [" "]
      - id: reason_phrase
        type: str
      - id: crlf
        type: str
        len: 2
        enum: ["\r\n"]

instances:
  has_body:
    value: content_length > 0

  content_length:
    valueof:
      - id: content_length
        type: uint
        if: field_name == "Content-Length"
        switch-on: field_name
        cases:
          "Content-Length": uint
          "Transfer-Encoding": str

  expr:
    value: true