meta:
  id: http1_1
  title: HTTP 1.1 Protocol
  application: Web Browsers, Web Servers, Proxies
  xref:
    rfc: 2616
doc: |
  HTTP/1.1 protocol specification according to RFC 2616, which includes definitions for methods, headers, and the body of requests and responses.

seq:
  - id: request
    type: http_request

  - id: response
    type: http_response

types:
  http_request:
    seq:
      - id: method
        type: str_enum
        enum: http_methods
      - id: uri
        type: str
        encoding: ASCII
      - id: version
        type: str_enum
        enum: http_versions
      - id: headers
        type: http_header
        repeat: eos

  http_response:
    seq:
      - id: version
        type: str_enum
        enum: http_versions
      - id: status_code
        type: u3
      - id: reason_phrase
        type: str
        encoding: ASCII
      - id: headers
        type: http_header
        repeat: eos
      - id: body
        type: http_body
        if: '!(version.as_enum == http_versions::HTTP_1_1 and status_code in [100, 101, 204, 205, 304])'

  http_header:
    seq:
      - id: name
        type: str
        encoding: ASCII
      - id: value
        type: str
        encoding: ASCII

  http_body:
    seq:
      - id: content
        size-eos: true

types:
  str_enum:
    seq:
      - id: data
        type: strz
        encoding: ASCII
    instances:
      as_enum:
        value: data

enums:
  http_methods:
    GET: "GET"
    POST: "POST"
    PUT: "PUT"
    DELETE: "DELETE"
    OPTIONS: "OPTIONS"
    HEAD: "HEAD"
    TRACE: "TRACE"
    CONNECT: "CONNECT"

  http_versions:
    HTTP_1_0: "HTTP/1.0"
    HTTP_1_1: "HTTP/1.1"