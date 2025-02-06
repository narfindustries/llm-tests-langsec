meta:
  id: http_1_1
  title: HTTP/1.1 Protocol Specification
  file-extension: http
  endian: be
  encoding: utf-8

seq:
  - id: request_or_response
    type: http_message

types:
  http_message:
    seq:
      - id: start_line
        type:
          switch-on: _root.message_type
          cases:
            'request': request_line
            'response': status_line
      - id: headers
        type: header_collection
      - id: body
        type: body_content
        size-eos: true

  request_line:
    seq:
      - id: method
        type: method_enum
      - id: uri
        type: str
        encoding: ascii
      - id: http_version
        type: str
        encoding: ascii
        pattern: 'HTTP/1\.1'

  status_line:
    seq:
      - id: http_version
        type: str
        encoding: ascii
        pattern: 'HTTP/1\.1'
      - id: status_code
        type: u2
        enum: http_status_codes
      - id: reason_phrase
        type: str
        encoding: ascii

  method_enum:
    seq:
      - id: value
        type: str
        encoding: ascii
        valid:
          any-of: 
            - 'GET'
            - 'POST'
            - 'HEAD'
            - 'PUT'
            - 'DELETE'
            - 'TRACE'
            - 'OPTIONS'
            - 'CONNECT'

  header_collection:
    seq:
      - id: headers
        type: header
        repeat: eos

  header:
    seq:
      - id: name
        type: str
        encoding: ascii
      - id: value
        type: str
        encoding: ascii

  authorization_value:
    seq:
      - id: auth_type
        type: auth_type_enum
      - id: credentials
        type: str
        encoding: ascii

  auth_type_enum:
    seq:
      - id: value
        type: str
        encoding: ascii
        valid:
          any-of:
            - 'Basic'
            - 'Digest'
            - 'Bearer'
            - 'OAuth'

  body_content:
    seq:
      - id: raw_body
        type: bytes
        size-eos: true

enums:
  http_status_codes:
    100: continue
    101: switching_protocols
    200: ok
    201: created
    204: no_content
    301: moved_permanently
    302: found
    304: not_modified
    400: bad_request
    401: unauthorized
    403: forbidden
    404: not_found
    500: internal_server_error
    501: not_implemented
    503: service_unavailable

instances:
  message_type:
    value: >-
      _io.pos == 0 and 
      (_root.start_line.method.value != '' ? 'request' : 'response')