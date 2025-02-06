meta:
  id: http_1_1
  file-extension: http
  endian: be
  encoding: ascii

seq:
  - id: first_byte
    size: 1
  - id: request_or_response
    type:
      switch-on: first_byte
      cases:
        '[0x47]': http_request  # 'G'
        '[0x50]': http_request  # 'P'
        '[0x44]': http_request  # 'D'
        '[0x4f]': http_request  # 'O'
        '[0x54]': http_request  # 'T'
        '[0x43]': http_request  # 'C'
        '[0x48]': http_message  # 'H'

types:
  http_message:
    seq:
      - id: remaining_first_line
        type: str
        terminator: 0x0d
        encoding: ascii
      - id: newline1
        contents: [0x0a]
      - id: headers
        type: header
        repeat: until
        repeat-until: _.is_empty
      - id: body
        type: str
        size-eos: true
        encoding: ascii
        if: has_body
    instances:
      is_response:
        value: "remaining_first_line.startswith('TTP/')"
      has_body:
        value: >-
          headers.any(
            _.name.lower == "content-length" or
            _.name.lower == "transfer-encoding"
          )

  http_request:
    seq:
      - id: method_remaining
        type: str
        terminator: 0x20
        encoding: ascii
      - id: uri
        type: str
        terminator: 0x20
        encoding: ascii
      - id: version
        type: str
        terminator: 0x0d
        encoding: ascii
      - id: newline
        contents: [0x0a]
      - id: headers
        type: header
        repeat: until
        repeat-until: _.is_empty
      - id: body
        type: str
        size-eos: true
        encoding: ascii
        if: has_body
    instances:
      has_body:
        value: >-
          headers.any(
            _.name.lower == "content-length" or
            _.name.lower == "transfer-encoding"
          )

  header:
    seq:
      - id: name
        type: str
        terminator: 0x3a
        encoding: ascii
      - id: space
        contents: [0x20]
      - id: value
        type: str
        terminator: 0x0d
        encoding: ascii
      - id: newline
        contents: [0x0a]
    instances:
      is_empty:
        value: name.length == 0

instances:
  method:
    value: >-
      first_byte == 0x48 and not request_or_response.is_response ?
        'HEAD' :
      first_byte == 0x47 ? 'GET' :
      first_byte == 0x50 and request_or_response.method_remaining.startswith('OST') ? 'POST' :
      first_byte == 0x50 and request_or_response.method_remaining.startswith('UT') ? 'PUT' :
      first_byte == 0x44 ? 'DELETE' :
      first_byte == 0x4f ? 'OPTIONS' :
      first_byte == 0x54 ? 'TRACE' :
      first_byte == 0x43 ? 'CONNECT' :
      'UNKNOWN'

  status_code:
    value: >-
      request_or_response.is_response ?
        request_or_response.remaining_first_line.substring(5, 8).to_i : 0

  status_message:
    value: >-
      request_or_response.is_response ?
        request_or_response.remaining_first_line.substring(9) : ''

enums:
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