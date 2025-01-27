meta:
  id: http_1_1
  title: HTTP/1.1 Protocol
  license: CC0-1.0
  endian: be
  file-extension: http
  encoding: UTF-8

seq:
  - id: request_line
    type: request_line
  - id: headers
    type: headers
  - id: body
    type: body

types:
  request_line:
    seq:
      - id: method
        type: str
        size: 8
      - id: uri
        type: str
        size: 256
      - id: version
        type: str
        size: 8

  headers:
    seq:
      - id: header_fields
        type: header_field
        repeat: eos

  header_field:
    seq:
      - id: name
        type: str
        size: 64
      - id: value
        type: str
        size: 256

  body:
    seq:
      - id: content
        type: str
        size-eos: true