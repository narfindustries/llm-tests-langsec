meta:
  id: http_1_1
  title: HTTP/1.1 Protocol
  license: MIT
  ks-version: 0.9
  endian: be
  imports:
    - /common/byte_array
seq:
  - id: request_line
    type: request_line
  - id: headers
    type: header
    repeat: eos
  - id: body
    size-eos: true
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
        size: 1024
      - id: version
        type: str
        encoding: ASCII
        size: 8
  header:
    seq:
      - id: name
        type: str
        encoding: ASCII
        size: 256
      - id: value
        type: str
        encoding: ASCII
        size: 1024