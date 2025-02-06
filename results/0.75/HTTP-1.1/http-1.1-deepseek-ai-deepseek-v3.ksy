meta:
  id: http
  title: HTTP 1.1
  license: CC0-1.0
  endian: be
  encoding: UTF-8
seq:
  - id: request_line
    type: request_line
    if: _root.is_request
  - id: response_line
    type: response_line
    if: not _root.is_request
  - id: headers
    type: header
    repeat: eos
  - id: body
    size-eos: true
types:
  request_line:
    seq:
      - id: method
        type: str
        encoding: ASCII
        size: 3
      - id: space1
        type: str
        encoding: ASCII
        size: 1
      - id: uri
        type: str
        encoding: ASCII
        size: until: "\x20"
      - id: space2
        type: str
        encoding: ASCII
        size: 1
      - id: http_version
        type: str
        encoding: ASCII
        size: until: "\x0D"
      - id: crlf
        type: str
        encoding: ASCII
        size: 2
  response_line:
    seq:
      - id: http_version
        type: str
        encoding: ASCII
        size: until: "\x20"
      - id: space1
        type: str
        encoding: ASCII
        size: 1
      - id: status_code
        type: u2
      - id: space2
        type: str
        encoding: ASCII
        size: 1
      - id: reason_phrase
        type: str
        encoding: ASCII
        size: until: "\x0D"
      - id: crlf
        type: str
        encoding: ASCII
        size: 2
  header:
    seq:
      - id: name
        type: str
        encoding: ASCII
        size: until: "\x3A"
      - id: colon
        type: str
        encoding: ASCII
        size: 1
      - id: space
        type: str
        encoding: ASCII
        size: 1
      - id: value
        type: str
        encoding: ASCII
        size: until: "\x0D"
      - id: crlf
        type: str
        encoding: ASCII
        size: 2
  body:
    seq:
      - id: data
        type: u1
        repeat: eos
enums:
  method:
    0x474554: GET
    0x484541: HEAD
    0x504F53: POST
    0x505554: PUT
    0x44454C: DELETE
    0x434F4E: CONNECT
    0x4F5054: OPTIONS
    0x545241: TRACE
  status_code:
    100: Continue
    101: Switching Protocols
    200: OK
    201: Created
    202: Accepted
    203: Non-Authoritative Information
    204: No Content
    205: Reset Content
    206: Partial Content
    300: Multiple Choices
    301: Moved Permanently
    302: Found
    303: See Other
    304: Not Modified
    305: Use Proxy
    307: Temporary Redirect
    400: Bad Request
    401: Unauthorized
    402: Payment Required
    403: Forbidden
    404: Not Found
    405: Method Not Allowed
    406: Not Acceptable
    407: Proxy Authentication Required
    408: Request Timeout
    409: Conflict
    410: Gone
    411: Length Required
    412: Precondition Failed
    413: Request Entity Too Large
    414: Request-URI Too Long
    415: Unsupported Media Type
    416: Requested Range Not Satisfiable
    417: Expectation Failed
    500: Internal Server Error
    501: Not Implemented
    502: Bad Gateway
    503: Service Unavailable
    504: Gateway Timeout
    505: HTTP Version Not Supported