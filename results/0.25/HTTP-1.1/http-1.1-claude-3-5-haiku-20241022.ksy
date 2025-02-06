meta:
  id: http_1_1
  title: HTTP/1.1 Protocol
  file-extension: http
  encoding: ascii
  endian: be

seq:
  - id: message
    type: message

types:
  message:
    seq:
      - id: message_type
        type: str
        size: 1
      - id: content
        type:
          switch-on: message_type
          cases:
            '"R"': request_message
            '"P"': response_message

  request_message:
    seq:
      - id: request_line
        type: request_line
      - id: headers
        type: headers
      - id: body
        type: body
        size-eos: true

  response_message:
    seq:
      - id: status_line
        type: status_line
      - id: headers
        type: headers
      - id: body
        type: body
        size-eos: true

  request_line:
    seq:
      - id: method
        type: str
        terminator: 0x20
        encoding: ascii
      - id: request_uri
        type: str
        terminator: 0x20
        encoding: ascii
      - id: http_version
        type: str
        terminator: 0x0D
        encoding: ascii

  status_line:
    seq:
      - id: http_version
        type: str
        terminator: 0x20
        encoding: ascii
      - id: status_code
        type: str
        terminator: 0x20
        encoding: ascii
      - id: reason_phrase
        type: str
        terminator: 0x0D
        encoding: ascii

  headers:
    seq:
      - id: general_headers
        type: general_headers
      - id: request_headers
        type: request_headers
      - id: response_headers
        type: response_headers
      - id: entity_headers
        type: entity_headers

  general_headers:
    seq:
      - id: cache_control
        type: header_field
        if: true
      - id: connection
        type: header_field
        if: true
      - id: date
        type: header_field
        if: true
      - id: pragma
        type: header_field
        if: true
      - id: trailer
        type: header_field
        if: true
      - id: transfer_encoding
        type: header_field
        if: true
      - id: upgrade
        type: header_field
        if: true
      - id: via
        type: header_field
        if: true
      - id: warning
        type: header_field
        if: true

  request_headers:
    seq:
      - id: accept
        type: header_field
        if: true
      - id: accept_charset
        type: header_field
        if: true
      - id: accept_encoding
        type: header_field
        if: true
      - id: accept_language
        type: header_field
        if: true
      - id: authorization
        type: header_field
        if: true
      - id: expect
        type: header_field
        if: true
      - id: from
        type: header_field
        if: true
      - id: host
        type: header_field
        if: true
      - id: if_match
        type: header_field
        if: true
      - id: if_modified_since
        type: header_field
        if: true
      - id: if_none_match
        type: header_field
        if: true
      - id: if_range
        type: header_field
        if: true
      - id: if_unmodified_since
        type: header_field
        if: true
      - id: max_forwards
        type: header_field
        if: true
      - id: proxy_authorization
        type: header_field
        if: true
      - id: range
        type: header_field
        if: true
      - id: referer
        type: header_field
        if: true
      - id: te
        type: header_field
        if: true
      - id: user_agent
        type: header_field
        if: true

  response_headers:
    seq:
      - id: accept_ranges
        type: header_field
        if: true
      - id: age
        type: header_field
        if: true
      - id: etag
        type: header_field
        if: true
      - id: location
        type: header_field
        if: true
      - id: proxy_authenticate
        type: header_field
        if: true
      - id: retry_after
        type: header_field
        if: true
      - id: server
        type: header_field
        if: true
      - id: vary
        type: header_field
        if: true
      - id: www_authenticate
        type: header_field
        if: true

  entity_headers:
    seq:
      - id: allow
        type: header_field
        if: true
      - id: content_encoding
        type: header_field
        if: true
      - id: content_language
        type: header_field
        if: true
      - id: content_length
        type: header_field
        if: true
      - id: content_location
        type: header_field
        if: true
      - id: content_md5
        type: header_field
        if: true
      - id: content_range
        type: header_field
        if: true
      - id: content_type
        type: header_field
        if: true
      - id: expires
        type: header_field
        if: true
      - id: last_modified
        type: header_field
        if: true

  header_field:
    seq:
      - id: name
        type: str
        terminator: 0x3A
        encoding: ascii
      - id: value
        type: str
        terminator: 0x0D
        encoding: ascii

  body:
    seq:
      - id: content
        type: str
        size-eos: true
        encoding: ascii

enums:
  http_methods:
    1: get
    2: post
    3: head
    4: put
    5: delete
    6: trace
    7: options
    8: connect
    9: patch

  http_versions:
    1: http_1_0
    2: http_1_1

  http_status_codes:
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