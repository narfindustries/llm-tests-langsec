meta:
  id: http_1_1
  title: HTTP/1.1 Protocol
  file-extension: http
  endian: be

doc: |
  HTTP/1.1 protocol specification based on RFC 2616.

seq:
  - id: message
    type: message

types:
  message:
    seq:
      - id: first_line
        type: str
        encoding: ASCII
        terminator: 0x0a
      - id: headers
        type: header
        repeat: until
        repeat-until: _.name == ''
      - id: body
        type: body
        if: has_body
    instances:
      is_request:
        value: "first_line.starts_with('GET') or 
                first_line.starts_with('POST') or 
                first_line.starts_with('HEAD') or
                first_line.starts_with('PUT') or
                first_line.starts_with('DELETE') or
                first_line.starts_with('CONNECT') or
                first_line.starts_with('OPTIONS') or
                first_line.starts_with('TRACE')"
      has_body:
        value: "headers.any(_.name.lower == 'content-length' or _.name.lower == 'transfer-encoding')"

  request_line:
    seq:
      - id: method
        type: str
        encoding: ASCII
        terminator: 0x20
      - id: request_uri
        type: str
        encoding: ASCII
        terminator: 0x20
      - id: http_version
        type: str
        encoding: ASCII
        terminator: 0x0a
    enums:
      method_enum:
        1: get
        2: post
        3: head
        4: put
        5: delete
        6: connect
        7: options
        8: trace

  status_line:
    seq:
      - id: http_version
        type: str
        encoding: ASCII
        terminator: 0x20
      - id: status_code
        type: u2
      - id: reason_phrase
        type: str
        encoding: ASCII
        terminator: 0x0a
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

  header:
    seq:
      - id: name
        type: str
        encoding: ASCII
        terminator: 0x3a
      - id: space
        type: str
        encoding: ASCII
        terminator: 0x20
      - id: value
        type: str
        encoding: ASCII
        terminator: 0x0a
        eos-error: false
    enums:
      header_types:
        1: accept
        2: accept_charset
        3: accept_encoding
        4: accept_language
        5: accept_ranges
        6: age
        7: allow
        8: authorization
        9: cache_control
        10: connection
        11: content_encoding
        12: content_language
        13: content_length
        14: content_location
        15: content_md5
        16: content_range
        17: content_type
        18: date
        19: etag
        20: expect
        21: expires
        22: from
        23: host
        24: if_match
        25: if_modified_since
        26: if_none_match
        27: if_range
        28: if_unmodified_since
        29: last_modified
        30: location
        31: max_forwards
        32: pragma
        33: proxy_authenticate
        34: proxy_authorization
        35: range
        36: referer
        37: retry_after
        38: server
        39: te
        40: trailer
        41: transfer_encoding
        42: upgrade
        43: user_agent
        44: vary
        45: via
        46: warning
        47: www_authenticate

  body:
    seq:
      - id: content
        size-eos: true
        type: str
        encoding: ASCII