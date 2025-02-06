meta:
  id: http
  title: HTTP/1.1
  file-extension: http
  encoding: UTF-8
  endian: be
seq:
  - id: request_line
    type: request_line
    if: _root.is_request
  - id: response_line
    type: response_line
    if: _root.is_response
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
  response_line:
    seq:
      - id: version
        type: str
        encoding: ASCII
        terminator: 0x20
      - id: status_code
        type: u2
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
    value: request_line != null
  is_response:
    value: response_line != null
  content_length:
    value: headers.any { |h| h.name == "Content-Length" } ? headers.find { |h| h.name == "Content-Length" }.value.to_i : 0