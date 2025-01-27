meta:
  id: http_message
  file-extension: bin
  endian: be
seq:
  - id: start_line
    type: start_line
  - id: headers
    type: header
    repeat: until
    repeat-until: _.name == ''
  - id: body
    type: body
    if: not _io.eof
types:
  start_line:
    seq:
      - id: method_or_version
        type: str
        encoding: ASCII
        terminator: 0x20
      - id: request_target_or_status
        type: str
        encoding: ASCII
        terminator: 0x20
      - id: version_or_reason
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
        if: name != ''
      - id: value
        type: str
        encoding: ASCII
        terminator: 0x0a
        if: name != ''
  body:
    seq:
      - id: content
        size-eos: true