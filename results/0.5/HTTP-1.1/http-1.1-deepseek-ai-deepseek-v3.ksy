meta:
  id: http
  title: HTTP/1.1
  file-extension: http
  license: MIT
  endian: be
seq:
  - id: request_line
    type: request_line
    if: _root.is_request
  - id: status_line
    type: status_line
    if: not _root.is_request
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
        terminator: 0x20
      - id: uri
        type: str
        encoding: ASCII
        terminator: 0x20
      - id: version
        type: str
        encoding: ASCII
        terminator: 0x0D
      - id: lf
        contents: 0x0A
  status_line:
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
      - id: lf
        contents: 0x0A
  header:
    seq:
      - id: name
        type: str
        encoding: ASCII
        terminator: 0x3A
      - id: value
        type: str
        encoding: ASCII
        terminator: 0x0D
      - id: lf
        contents: 0x0A
instances:
  is_request:
    value: _root.request_line.method != ""
  content_length:
    value: >
      headers.find { it.name == "Content-Length" } != null ?
      headers.find { it.name == "Content-Length" }.value.to_i : 0