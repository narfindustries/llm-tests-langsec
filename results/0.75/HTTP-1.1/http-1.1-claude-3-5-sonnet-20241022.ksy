meta:
  id: http_response
  title: HTTP/1.1 Response
  file-extension: bin
  endian: be

seq:
  - id: status_line
    type: status_line
  - id: headers
    type: header
    repeat: until
    repeat-until: _.name == ""
  - id: body
    size-eos: true
    if: not _io.eof

types:
  status_line:
    seq:
      - id: protocol
        type: str
        encoding: ASCII
        terminator: 0x20
      - id: status_code
        type: str
        encoding: ASCII
        size: 3
      - id: space
        type: u1
      - id: status_message
        type: str
        encoding: ASCII
        terminator: 0x0a

  header:
    seq:
      - id: name
        type: str
        encoding: ASCII
        terminator: 0x3a
      - id: space
        type: u1
        if: name != ""
      - id: value
        type: str
        encoding: ASCII
        terminator: 0x0a
        if: name != ""