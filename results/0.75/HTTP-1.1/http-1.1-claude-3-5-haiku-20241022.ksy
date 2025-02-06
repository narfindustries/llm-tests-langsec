meta:
  id: http_1_1
  title: HTTP/1.1 Protocol
  file-extension: http
  endian: be

seq:
  - id: request_or_response
    type:
      switch-on: is_request
      cases:
        true: http_request
        false: http_response

types:
  http_request:
    seq:
      - id: method
        type: str
        encoding: ascii
        terminator: 32
      - id: uri
        type: strz
        encoding: ascii
      - id: http_version
        type: str
        encoding: ascii
        terminator: 13
        consume: false
      - id: headers
        type: header
        repeat: eos
      - id: body
        type: body
        size-eos: true

  http_response:
    seq:
      - id: status_line
        type: status_line
      - id: headers
        type: header
        repeat: eos
      - id: body
        type: body
        size-eos: true

  status_line:
    seq:
      - id: http_version
        type: str
        encoding: ascii
        terminator: 32
      - id: status_code
        type: str
        terminator: 32
        encoding: ascii
      - id: reason_phrase
        type: strz
        encoding: ascii

  header:
    seq:
      - id: name
        type: strz
        encoding: ascii
      - id: value
        type: strz
        encoding: ascii

  body:
    seq:
      - id: content
        type: bytes
        size-eos: true

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

  http_status_codes:
    100: continue
    101: switching_protocols
    200: ok
    201: created
    202: accepted
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
    502: bad_gateway
    503: service_unavailable

instances:
  is_request:
    value: true
    if: request_or_response.http_request.method != ""

  content_type:
    value: request_or_response.headers[0].value
    if: request_or_response.headers[0].name == "Content-Type"

  content_length:
    value: request_or_response.headers[0].value.to_i
    if: request_or_response.headers[0].name == "Content-Length"

  transfer_encoding:
    value: request_or_response.headers[0].value
    if: request_or_response.headers[0].name == "Transfer-Encoding"

  cache_control:
    value: request_or_response.headers[0].value
    if: request_or_response.headers[0].name == "Cache-Control"

  connection:
    value: request_or_response.headers[0].value
    if: request_or_response.headers[0].name == "Connection"

  date:
    value: request_or_response.headers[0].value
    if: request_or_response.headers[0].name == "Date"

  accept:
    value: request_or_response.headers[0].value
    if: request_or_response.headers[0].name == "Accept"

  accept_charset:
    value: request_or_response.headers[0].value
    if: request_or_response.headers[0].name == "Accept-Charset"

  accept_encoding:
    value: request_or_response.headers[0].value
    if: request_or_response.headers[0].name == "Accept-Encoding"

  accept_language:
    value: request_or_response.headers[0].value
    if: request_or_response.headers[0].name == "Accept-Language"

  authorization:
    value: request_or_response.headers[0].value
    if: request_or_response.headers[0].name == "Authorization"

  host:
    value: request_or_response.headers[0].value
    if: request_or_response.headers[0].name == "Host"

  user_agent:
    value: request_or_response.headers[0].value
    if: request_or_response.headers[0].name == "User-Agent"

  age:
    value: request_or_response.headers[0].value.to_i
    if: request_or_response.headers[0].name == "Age"

  etag:
    value: request_or_response.headers[0].value
    if: request_or_response.headers[0].name == "ETag"

  location:
    value: request_or_response.headers[0].value
    if: request_or_response.headers[0].name == "Location"

  server:
    value: request_or_response.headers[0].value
    if: request_or_response.headers[0].name == "Server"

  www_authenticate:
    value: request_or_response.headers[0].value
    if: request_or_response.headers[0].name == "WWW-Authenticate"

  content_encoding:
    value: request_or_response.headers[0].value
    if: request_or_response.headers[0].name == "Content-Encoding"

  content_language:
    value: request_or_response.headers[0].value
    if: request_or_response.headers[0].name == "Content-Language"

  expires:
    value: request_or_response.headers[0].value
    if: request_or_response.headers[0].name == "Expires"

  if_match:
    value: request_or_response.headers[0].value
    if: request_or_response.headers[0].name == "If-Match"

  if_modified_since:
    value: request_or_response.headers[0].value
    if: request_or_response.headers[0].name == "If-Modified-Since"

  if_none_match:
    value: request_or_response.headers[0].value
    if: request_or_response.headers[0].name == "If-None-Match"

  if_unmodified_since:
    value: request_or_response.headers[0].value
    if: request_or_response.headers[0].name == "If-Unmodified-Since"