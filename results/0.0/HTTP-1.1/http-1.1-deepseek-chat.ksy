meta:
  id: http_1_1
  title: HTTP/1.1 Protocol
  license: CC0-1.0
  ks-version: 0.9
  endian: be
  imports:
    - /common/byte_array

seq:
  - id: request_line
    type: request_line
  - id: headers
    type: header
    repeat: until
    repeat-until: _.name == ""
  - id: body
    size: _root._io.size - _root._io.pos

types:
  request_line:
    seq:
      - id: method
        type: str
        encoding: ASCII
        size: 20
      - id: uri
        type: str
        encoding: ASCII
        size: 100
      - id: version
        type: str
        encoding: ASCII
        size: 10
      - id: crlf
        contents: "\r\n"

  header:
    seq:
      - id: name
        type: str
        encoding: ASCII
        size: 50
      - id: separator
        contents: ": "
      - id: value
        type: str
        encoding: ASCII
        size: 100
      - id: crlf
        contents: "\r\n"