meta:
  id: http
  title: HTTP/1.1
  license: CC0-1.0
  endian: be
seq:
  - id: request_line
    type: request_line
    if: _root.is_request
  - id: response_line
    type: response_line
    if: _root.is_response
  - id: headers
    type: header
    repeat: until
    repeat-until: _.name == ""
  - id: body
    size: _root.content_length
    if: _root.content_length != 0
types:
  request_line:
    seq:
      - id: method
        type: str
        encoding: ASCII
        size: 8
      - id: uri
        type: str
        encoding: ASCII
        size: 256
      - id: version
        type: str
        encoding: ASCII
        size: 8
  response_line:
    seq:
      - id: version
        type: str
        encoding: ASCII
        size: 8
      - id: status_code
        type: u2
      - id: reason_phrase
        type: str
        encoding: ASCII
        size: 256
  header:
    seq:
      - id: name
        type: str
        encoding: ASCII
        size: 64
      - id: value
        type: str
        encoding: ASCII
        size: 1024
  general_headers:
    seq:
      - id: cache_control
        type: str
        encoding: ASCII
        size: 256
        if: _root.headers.any { it.name == "Cache-Control" }
      - id: connection
        type: str
        encoding: ASCII
        size: 64
        if: _root.headers.any { it.name == "Connection" }
      - id: date
        type: str
        encoding: ASCII
        size: 64
        if: _root.headers.any { it.name == "Date" }
      - id: pragma
        type: str
        encoding: ASCII
        size: 64
        if: _root.headers.any { it.name == "Pragma" }
      - id: trailer
        type: str
        encoding: ASCII
        size: 256
        if: _root.headers.any { it.name == "Trailer" }
      - id: transfer_encoding
        type: str
        encoding: ASCII
        size: 64
        if: _root.headers.any { it.name == "Transfer-Encoding" }
      - id: upgrade
        type: str
        encoding: ASCII
        size: 64
        if: _root.headers.any { it.name == "Upgrade" }
      - id: via
        type: str
        encoding: ASCII
        size: 256
        if: _root.headers.any { it.name == "Via" }
      - id: warning
        type: str
        encoding: ASCII
        size: 256
        if: _root.headers.any { it.name == "Warning" }
  request_headers:
    seq:
      - id: accept
        type: str
        encoding: ASCII
        size: 256
        if: _root.headers.any { it.name == "Accept" }
      - id: accept_charset
        type: str
        encoding: ASCII
        size: 256
        if: _root.headers.any { it.name == "Accept-Charset" }
      - id: accept_encoding
        type: str
        encoding: ASCII
        size: 256
        if: _root.headers.any { it.name == "Accept-Encoding" }
      - id: accept_language
        type: str
        encoding: ASCII
        size: 256
        if: _root.headers.any { it.name == "Accept-Language" }
      - id: authorization
        type: str
        encoding: ASCII
        size: 256
        if: _root.headers.any { it.name == "Authorization" }
      - id: expect
        type: str
        encoding: ASCII
        size: 64
        if: _root.headers.any { it.name == "Expect" }
      - id: from
        type: str
        encoding: ASCII
        size: 64
        if: _root.headers.any { it.name == "From" }
      - id: host
        type: str
        encoding: ASCII
        size: 64
        if: _root.headers.any { it.name == "Host" }
      - id: if_match
        type: str
        encoding: ASCII
        size: 256
        if: _root.headers.any { it.name == "If-Match" }
      - id: if_modified_since
        type: str
        encoding: ASCII
        size: 64
        if: _root.headers.any { it.name == "If-Modified-Since" }
      - id: if_none_match
        type: str
        encoding: ASCII
        size: 256
        if: _root.headers.any { it.name == "If-None-Match" }
      - id: if_range
        type: str
        encoding: ASCII
        size: 64
        if: _root.headers.any { it.name == "If-Range" }
      - id: if_unmodified_since
        type: str
        encoding: ASCII
        size: 64
        if: _root.headers.any { it.name == "If-Unmodified-Since" }
      - id: max_forwards
        type: u2
        if: _root.headers.any { it.name == "Max-Forwards" }
      - id: proxy_authorization
        type: str
        encoding: ASCII
        size: 256
        if: _root.headers.any { it.name == "Proxy-Authorization" }
      - id: range
        type: str
        encoding: ASCII
        size: 256
        if: _root.headers.any { it.name == "Range" }
      - id: referer
        type: str
        encoding: ASCII
        size: 256
        if: _root.headers.any { it.name == "Referer" }
      - id: te
        type: str
        encoding: ASCII
        size: 64
        if: _root.headers.any { it.name == "TE" }
      - id: user_agent
        type: str
        encoding: ASCII
        size: 256
        if: _root.headers.any { it.name == "User-Agent" }
  response_headers:
    seq:
      - id: accept_ranges
        type: str
        encoding: ASCII
        size: 64
        if: _root.headers.any { it.name == "Accept-Ranges" }
      - id: age
        type: u4
        if: _root.headers.any { it.name == "Age" }
      - id: etag
        type: str
        encoding: ASCII
        size: 256
        if: _root.headers.any { it.name == "ETag" }
      - id: location
        type: str
        encoding: ASCII
        size: 256
        if: _root.headers.any { it.name == "Location" }
      - id: proxy_authenticate
        type: str
        encoding: ASCII
        size: 256
        if: _root.headers.any { it.name == "Proxy-Authenticate" }
      - id: retry_after
        type: str
        encoding: ASCII
        size: 64
        if: _root.headers.any { it.name == "Retry-After" }
      - id: server
        type: str
        encoding: ASCII
        size: 256
        if: _root.headers.any { it.name == "Server" }
      - id: vary
        type: str
        encoding: ASCII
        size: 256
        if: _root.headers.any { it.name == "Vary" }
      - id: www_authenticate
        type: str
        encoding: ASCII
        size: 256
        if: _root.headers.any { it.name == "WWW-Authenticate" }
  entity_headers:
    seq:
      - id: allow
        type: str
        encoding: ASCII
        size: 256
        if: _root.headers.any { it.name == "Allow" }
      - id: content_encoding
        type: str
        encoding: ASCII
        size: 64
        if: _root.headers.any { it.name == "Content-Encoding" }
      - id: content_language
        type: str
        encoding: ASCII
        size: 64
        if: _root.headers.any { it.name == "Content-Language" }
      - id: content_length
        type: u4
        if: _root.headers.any { it.name == "Content-Length" }
      - id: content_location
        type: str
        encoding: ASCII
        size: 256
        if: _root.headers.any { it.name == "Content-Location" }
      - id: content_md5
        type: str
        encoding: ASCII