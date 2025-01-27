meta:
  id: http_message
  title: HTTP 1.1 Message
  file-extension: http
  endian: be

seq:
  - id: start_line
    type: start_line
  - id: headers
    type: headers
  - id: body
    size-eos: true
    if: start_line.method == 'POST' or start_line.method == 'PUT' or start_line.method == 'PATCH'

types:
  start_line:
    seq:
      - id: method
        type: strz
        encoding: ASCII
        terminator: 0x20
      - id: uri
        type: strz
        encoding: ASCII
        terminator: 0x20
      - id: version
        type: strz
        encoding: ASCII
        terminator: 0x0d0a

  headers:
    seq:
      - id: entries
        type: header_entry
        repeat: until
        repeat-until: _.is_end_of_headers

  header_entry:
    seq:
      - id: name
        type: strz
        encoding: ASCII
        terminator: 0x3a
      - id: value
        type: strz
        encoding: ASCII
        terminator: 0x0d0a
    instances:
      is_end_of_headers:
        value: name == "" and value == ""