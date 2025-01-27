meta:
  id: http_message
  title: HTTP Message
  file-extension: http
  endian: be

seq:
  - id: start_line
    type: start_line
  - id: headers
    type: header
    repeat: until
    repeat-until: _.is_empty
  - id: body
    size-eos: true
    if: start_line.method != "GET"  # Typically, GET requests do not have a body

types:
  start_line:
    seq:
      - id: method
        type: strz
        encoding: ascii
        terminator: 0x20  # Space
      - id: uri
        type: strz
        encoding: ascii
        terminator: 0x20  # Space
      - id: version
        type: strz
        encoding: ascii
        terminator: 0x0d0a  # CRLF

  header:
    seq:
      - id: name
        type: strz
        encoding: ascii
        terminator: 0x3a  # Colon
      - id: value
        type: strz
        encoding: ascii
        terminator: 0x0d0a  # CRLF

    instances:
      is_empty:
        value: name == ""