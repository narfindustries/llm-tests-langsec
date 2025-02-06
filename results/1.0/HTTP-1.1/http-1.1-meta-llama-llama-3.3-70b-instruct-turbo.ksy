meta:
  id: http_1_1

types:
  request_line:
    seq:
      - id: method
        type: str
        encoding: ascii
        size: 4
      - id: sp1
        type: str
        encoding: ascii
        size: 1
      - id: request_uri
        type: str
        encoding: ascii
      - id: sp2
        type: str
        encoding: ascii
        size: 1
      - id: http_version
        type: str
        encoding: ascii
        size: 8

  header:
    seq:
      - id: field_name
        type: str
        encoding: ascii
      - id: colon
        type: str
        encoding: ascii
        size: 1
      - id: field_value
        type: str
        encoding: ascii
        size: until0

  headers:
    seq:
      - id: header
        type: header
        repeat: until

  request_body:
    seq:
      - id: body
        type: str
        encoding: ascii
        size: content_length

  response:
    seq:
      - id: http_version
        type: str
        encoding: ascii
        size: 8
      - id: sp1
        type: str
        encoding: ascii
        size: 1
      - id: status_code
        type: str
        encoding: ascii
        size: 3
      - id: sp2
        type: str
        encoding: ascii
        size: 1
      - id: reason_phrase
        type: str
        encoding: ascii
        size: until0

  response_header:
    seq:
      - id: field_name
        type: str
        encoding: ascii
      - id: colon
        type: str
        encoding: ascii
        size: 1
      - id: field_value
        type: str
        encoding: ascii
        size: until0

  response_headers:
    seq:
      - id: header
        type: response_header
        repeat: until

  response_body:
    seq:
      - id: body
        type: str
        encoding: ascii
        size: content_length

seq:
  - id: request_line
    type: request_line
  - id: headers
    type: headers
    repeat: until
  - id: request_body
    type: request_body

enums:
  methods:
    0: GET
    1: HEAD
    2: POST
    3: PUT
    4: DELETE
    5: CONNECT
    6: OPTIONS
    7: TRACE

  status_codes:
    100: Continue
    101: Switching Protocols
    200: OK
    201: Created
    202: Accepted
    203: Non-Authoritative Information
    204: No Content
    205: Reset Conent
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
    408: Request Time-out
    409: Conflict
    410: Gone
    411: Length Required
    412: Precondition Failed
    413: Request Entity Too Large
    414: Request-URI Too Large
    415: Unsupported Media Type
    416: Requested range not satisfiable
    417: Expectation Failed
    500: Internal Server Error
    501: Not Implemented
    502: Bad Gateway
    503: Service Unavailable
    504: Gateway Time-out
    505: HTTP Version not supported

instances:
  content_length:
    pos: headers
    check: field_name == "content-length"
    size: field_value.size

  request_method:
    pos: request_line
    check: method != ""
    size: method.size

  status_code:
    pos: response
    check: status_code != ""
    size: 3