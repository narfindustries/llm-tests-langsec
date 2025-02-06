meta:
  id: http_1_1
  endian: be
  encoding: utf-8

seq:
  - id: request_line
    type: request_line
  - id: headers
    type: header
    repeat: eos

types:
  request_line:
    seq:
      - id: method
        type: str
        terminator: 0x20
        encoding: utf-8
      - id: uri
        type: str
        terminator: 0x20
        encoding: utf-8
      - id: http_version
        type: str
        terminator: 0x0D0A
        encoding: utf-8

  header:
    seq:
      - id: name
        type: str
        terminator: 0x3A
        encoding: utf-8
      - id: value
        type: str
        terminator: 0x0D0A
        encoding: utf-8

  response:
    seq:
      - id: status_line
        type: status_line
      - id: headers
        type: header
        repeat: eos
      - id: body
        type: body

  status_line:
    seq:
      - id: http_version
        type: str
        terminator: 0x20
        encoding: utf-8
      - id: status_code
        type: u2
        enum: status_codes
      - id: reason_phrase
        type: str
        terminator: 0x0D0A
        encoding: utf-8

  body:
    seq:
      - id: content
        type: str
        size-eos: true
        encoding: utf-8

enums:
  http_methods:
    1: get
    2: post
    3: put
    4: delete
    5: head
    6: options
    7: trace
    8: connect
    9: patch

  header_names:
    1: accept
    2: accept_charset
    3: accept_encoding
    4: accept_language
    5: authorization
    6: host
    7: user_agent
    8: connection
    9: content_type
    10: content_length
    11: server

  status_codes:
    200: ok
    201: created
    204: no_content
    301: moved_permanently
    400: bad_request
    401: unauthorized
    403: forbidden
    404: not_found
    500: internal_server_error
    502: bad_gateway
    503: service_unavailable