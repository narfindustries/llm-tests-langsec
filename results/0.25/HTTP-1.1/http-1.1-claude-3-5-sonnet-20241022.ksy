meta:
  id: http_1_1
  title: HTTP/1.1 Protocol
  file-extension: http
  endian: be

seq:
  - id: request_or_response
    type:
      switch-on: _io.peek_str_to_byte(4)
      cases:
        '"HTTP"': response
        _: request

types:
  request:
    seq:
      - id: method
        type: str
        terminator: 0x20
        encoding: ASCII
      - id: request_uri
        type: str
        terminator: 0x20
        encoding: ASCII
      - id: http_version
        type: str
        terminator: 0x0d
        encoding: ASCII
      - id: newline1
        type: u1
      - id: headers
        type: header
        repeat: until
        repeat-until: _.name == ""
      - id: body
        type: str
        encoding: ASCII
        size-eos: true
        if: has_body
    instances:
      has_body:
        value: 'true'

  response:
    seq:
      - id: http_version
        type: str
        terminator: 0x20
        encoding: ASCII
      - id: status_code
        type: str
        size: 3
        encoding: ASCII
      - id: status_message
        type: str
        terminator: 0x0d
        encoding: ASCII
      - id: newline1
        type: u1
      - id: headers
        type: header
        repeat: until
        repeat-until: _.name == ""
      - id: body
        type: str
        encoding: ASCII
        size-eos: true
        if: has_body
    instances:
      has_body:
        value: 'true'

  header:
    seq:
      - id: name
        type: str
        terminator: 0x3a
        encoding: ASCII
      - id: space
        type: str
        terminator: 0x20
        encoding: ASCII
        if: name != ""
      - id: value
        type: str
        terminator: 0x0d
        encoding: ASCII
        if: name != ""
      - id: newline1
        type: u1
        if: name != ""

enums:
  methods:
    1: options
    2: get
    3: head
    4: post
    5: put
    6: delete
    7: trace
    8: connect

  status_codes:
    100: continue
    101: switching_protocols
    200: ok
    201: created
    202: accepted
    203: non_authoritative_information
    204: no_content
    205: reset_content
    206: partial_content
    300: multiple_choices
    301: moved_permanently
    302: found
    303: see_other
    304: not_modified
    305: use_proxy
    307: temporary_redirect
    400: bad_request
    401: unauthorized
    402: payment_required
    403: forbidden
    404: not_found
    405: method_not_allowed
    406: not_acceptable
    407: proxy_authentication_required
    408: request_timeout
    409: conflict
    410: gone
    411: length_required
    412: precondition_failed
    413: request_entity_too_large
    414: request_uri_too_long
    415: unsupported_media_type
    416: requested_range_not_satisfiable
    417: expectation_failed
    500: internal_server_error
    501: not_implemented
    502: bad_gateway
    503: service_unavailable
    504: gateway_timeout
    505: http_version_not_supported