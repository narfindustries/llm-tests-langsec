meta:
  id: http_1_1
  file-extension: bin
  endian: be

seq:
  - id: request_line
    type: request_line
  - id: headers
    type: header
    repeat: until
    repeat-until: _.name == ""
  - id: body
    type: str
    encoding: UTF-8
    size-eos: true
    if: not _io.eof

types:
  request_line:
    seq:
      - id: method
        type: str
        encoding: UTF-8
        terminator: 0x20
      - id: uri
        type: str
        encoding: UTF-8
        terminator: 0x20
      - id: version
        type: str
        encoding: UTF-8
        terminator: 0x0a

  header:
    seq:
      - id: name
        type: str
        encoding: UTF-8
        terminator: 0x3a
      - id: space
        type: str
        encoding: UTF-8
        terminator: 0x20
        if: name != ""
      - id: value
        type: str
        encoding: UTF-8
        terminator: 0x0a
        if: name != ""