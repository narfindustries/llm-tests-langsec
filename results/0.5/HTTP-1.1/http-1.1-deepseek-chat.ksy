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
        size: 16
        encoding: ASCII
      - id: uri
        type: str
        size: 256
        encoding: ASCII
      - id: version
        type: str
        size: 8
        encoding: ASCII
  header:
    seq:
      - id: name
        type: str
        size: 64
        encoding: ASCII
      - id: value
        type: str
        size: 256
        encoding: ASCII