meta:
  id: http_1_1
  title: HTTP/1.1 Protocol
  file-extension: http
  endian: be

seq:
  - id: message
    type:
      switch-on: _io.read_bytes(4)
      cases:
        '"GET "'  : http_request
        '"POST "' : http_request
        '"HEAD "' : http_request
        '"PUT "'  : http_request
        '"HTTP"'  : http_response
        _: http_request

types:
  http_request:
    seq:
      - id: method
        type: str
        terminator: 0x20
        encoding: ASCII
      - id: uri
        type: str
        terminator: 0x20
        encoding: ASCII
      - id: version
        type: str
        terminator: 0x0d
        encoding: ASCII
      - id: newline1
        contents: [0x0a]
      - id: headers
        type: header
        repeat: until
        repeat-until: _.name == ''
      - id: body
        type: body
        if: has_body
    instances:
      content_length_header:
        value: >-
          headers[0].name == "Content-Length" ? headers[0].value :
          headers[1].name == "Content-Length" ? headers[1].value : ""
      transfer_encoding_header:
        value: >-
          headers[0].name == "Transfer-Encoding" ? headers[0].value :
          headers[1].name == "Transfer-Encoding" ? headers[1].value : ""
      content_length_value:
        value: 'content_length_header.to_i'
      has_body:
        value: content_length_header != "" or transfer_encoding_header != ""
      is_chunked:
        value: transfer_encoding_header == "chunked"

  http_response:
    seq:
      - id: version
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
        contents: [0x0a]
      - id: headers
        type: header
        repeat: until
        repeat-until: _.name == ''
      - id: body
        type: body
        if: has_body
    instances:
      content_length_header:
        value: >-
          headers[0].name == "Content-Length" ? headers[0].value :
          headers[1].name == "Content-Length" ? headers[1].value : ""
      transfer_encoding_header:
        value: >-
          headers[0].name == "Transfer-Encoding" ? headers[0].value :
          headers[1].name == "Transfer-Encoding" ? headers[1].value : ""
      content_length_value:
        value: 'content_length_header.to_i'
      has_body:
        value: content_length_header != "" or transfer_encoding_header != ""
      is_chunked:
        value: transfer_encoding_header == "chunked"

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
        if: name != ''
      - id: value
        type: str
        terminator: 0x0d
        encoding: ASCII
        if: name != ''
      - id: newline
        contents: [0x0a]

  body:
    seq:
      - id: content
        size: _parent.content_length_value
        if: not _parent.is_chunked
      - id: chunked_content
        type: chunked_body
        if: _parent.is_chunked

  chunked_body:
    seq:
      - id: chunks
        type: chunk
        repeat: until
        repeat-until: _.size == 0

  chunk:
    seq:
      - id: size_hex
        type: str
        terminator: 0x0d
        encoding: ASCII
      - id: newline1
        contents: [0x0a]
      - id: content
        size: size
      - id: newline2
        contents: [0x0d, 0x0a]
    instances:
      size:
        value: 'size_hex.to_i(16)'

enums:
  http_methods:
    0x47455420: get
    0x504f5354: post
    0x48454144: head
    0x50555420: put
    0x44454c45: delete
    0x4f505449: options
    0x54524143: trace
    0x434f4e4e: connect

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