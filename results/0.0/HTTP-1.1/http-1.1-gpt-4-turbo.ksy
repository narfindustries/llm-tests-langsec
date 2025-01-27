meta:
  id: http_1_1
  title: HTTP 1.1 Protocol
  file-extension: http
  endian: be
  license: CC0-1.0
  ks-version: 0.9

doc: |
  A basic parser for the HTTP 1.1 protocol, focusing on the structure of HTTP requests and responses.

seq:
  - id: request
    type: http_request

  - id: response
    type: http_response

types:
  http_request:
    seq:
      - id: method
        type: str
        encoding: ASCII
        terminator: 0x20
      - id: uri
        type: str
        encoding: ASCII
        terminator: 0x20
      - id: version
        type: str
        encoding: ASCII
        terminator: 0x0D
      - id: newline
        size: 1
      - id: headers
        type: header
        repeat: until
        repeat-until: _.name == ""

  http_response:
    seq:
      - id: version
        type: str
        encoding: ASCII
        terminator: 0x20
      - id: status_code
        type: str
        encoding: ASCII
        terminator: 0x20
      - id: reason_phrase
        type: str
        encoding: ASCII
        terminator: 0x0D
      - id: newline
        size: 1
      - id: headers
        type: header
        repeat: until
        repeat-until: _.name == ""

  header:
    seq:
      - id: name
        type: str
        encoding: ASCII
        terminator: 0x3A
      - id: space
        size: 1
      - id: value
        type: str
        encoding: ASCII
        terminator: 0x0D
      - id: newline
        size: 1