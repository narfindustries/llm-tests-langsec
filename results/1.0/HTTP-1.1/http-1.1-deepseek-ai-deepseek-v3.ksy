meta:
  id: http
  title: HTTP/1.1
  file-extension: http
  license: MIT
  endian: be
  encoding: ASCII
seq:
  - id: request_line
    type: request_line
    if: _parent.is_request
  - id: status_line
    type: status_line
    if: _parent.is_response
  - id: headers
    type: headers
  - id: body
    type: body
    size: _root.headers.content_length
types:
  request_line:
    seq:
      - id: method
        type: str
        terminator: " "
        enum: method
      - id: uri
        type: str
        terminator: " "
      - id: version
        type: str
        terminator: "\r\n"
        enum: http_version
    enums:
      method:
        GET: GET
        HEAD: HEAD
        POST: POST
        PUT: PUT
        DELETE: DELETE
        CONNECT: CONNECT
        OPTIONS: OPTIONS
        TRACE: TRACE
      http_version:
        HTTP_1_0: HTTP/1.0
        HTTP_1_1: HTTP/1.1
  status_line:
    seq:
      - id: version
        type: str
        terminator: " "
        enum: http_version
      - id: status_code
        type: u2
      - id: reason_phrase
        type: str
        terminator: "\r\n"
  headers:
    seq:
      - id: header_fields
        type: header_field
        repeat: until
        repeat-until: _ == "\r\n"
    instances:
      content_length:
        value: >
          header_fields.find { it.name == "Content-Length" }?.value?.toInteger() ?: 0
    types:
      header_field:
        seq:
          - id: name
            type: str
            terminator: ": "
            enum: header_name
          - id: value
            type: str
            terminator: "\r\n"
        enums:
          header_name:
            Accept: Accept
            Accept-Charset: Accept-Charset
            Accept-Encoding: Accept-Encoding
            Accept-Language: Accept-Language
            Authorization: Authorization
            Cache-Control: Cache-Control
            Connection: Connection
            Content-Encoding: Content-Encoding
            Content-Language: Content-Language
            Content-Length: Content-Length
            Content-Location: Content-Location
            Content-MD5: Content-MD5
            Content-Range: Content-Range
            Content-Type: Content-Type
            Date: Date
            ETag: ETag
            Expect: Expect
            Expires: Expires
            From: From
            Host: Host
            If-Match: If-Match
            If-Modified-Since: If-Modified-Since
            If-None-Match: If-None-Match
            If-Range: If-Range
            If-Unmodified-Since: If-Unmodified-Since
            Last-Modified: Last-Modified
            Location: Location
            Max-Forwards: Max-Forwards
            Pragma: Pragma
            Proxy-Authenticate: Proxy-Authenticate
            Proxy-Authorization: Proxy-Authorization
            Range: Range
            Referer: Referer
            Retry-After: Retry-After
            Server: Server
            TE: TE
            Trailer: Trailer
            Transfer-Encoding: Transfer-Encoding
            Upgrade: Upgrade
            User-Agent: User-Agent
            Vary: Vary
            Via: Via
            Warning: Warning
            WWW-Authenticate: WWW-Authenticate
  body:
    seq:
      - id: content
        type: str
        size: _root.headers.content_length
instances:
  is_request:
    value: _root.request_line != null
  is_response:
    value: _root.status_line != null