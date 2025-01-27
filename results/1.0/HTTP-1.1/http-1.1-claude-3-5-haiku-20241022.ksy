meta:
  id: http_1_1
  title: HTTP/1.1 Protocol Specification
  file-extension: http
  license: MIT
  endian: be

seq:
  - id: method
    type: str
    encoding: ASCII
    terminator: 0x20
    doc: HTTP request method (GET, POST, etc.)

  - id: uri
    type: str
    encoding: ASCII
    terminator: 0x20
    doc: Requested URI path

  - id: http_version
    type: str
    encoding: ASCII
    terminator: 0x0D
    doc: HTTP protocol version

  - id: headers
    type: header
    repeat: until
    repeat-until: _.name.length == 0

  - id: body
    type: body
    doc: HTTP request/response body

types:
  header:
    seq:
      - id: name
        type: str
        encoding: ASCII
        terminator: 0x3A
        doc: Header name
      
      - id: value
        type: str
        encoding: ASCII
        terminator: 0x0D
        doc: Header value

  body:
    seq:
      - id: content_length
        type: u4
        doc: Length of body content
      
      - id: content
        type: str
        encoding: ASCII
        size: content_length
        doc: Actual body content