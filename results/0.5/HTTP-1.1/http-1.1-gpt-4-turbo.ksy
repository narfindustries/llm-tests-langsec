meta:
  id: http_1_1
  title: HTTP 1.1 Protocol
  file-extension: http
  endian: be
doc: |
  HTTP/1.1 protocol as defined by RFC 2616. This specification covers the basic structure
  of HTTP requests and responses.

seq:
  - id: request
    type: http_request
  - id: response
    type: http_response

types:
  http_request:
    seq:
      - id: method
        type: strz
      - id: uri
        type: strz
      - id: version
        type: strz
      - id: headers
        type: headers

  http_response:
    seq:
      - id: version
        type: strz
      - id: status_code
        type: strz
      - id: reason_phrase
        type: strz
      - id: headers
        type: headers

  headers:
    seq:
      - id: entries
        type: header
        repeat: eos

  header:
    seq:
      - id: name
        type: strz
      - id: value
        type: strz

  strz:
    seq:
      - id: str
        type: str
        terminator: 0x0D0A
        encoding: ASCII
