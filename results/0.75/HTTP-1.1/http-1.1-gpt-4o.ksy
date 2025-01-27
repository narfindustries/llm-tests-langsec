meta:
  id: http_1_1
  title: HTTP 1.1
  file-extension: http
  endian: be

seq:
  - id: request_line
    type: request_line
  - id: headers
    type: header
    repeat: until
    repeat-until: _.is_empty
  - id: body
    size: _root.content_length

types:
  request_line:
    seq:
      - id: method
        type: strz
        encoding: ascii
        terminator: 0x20
      - id: request_uri
        type: strz
        encoding: ascii
        terminator: 0x20
      - id: http_version
        type: strz
        encoding: ascii
        terminator: 0x0d0a

  header:
    seq:
      - id: name
        type: strz
        encoding: ascii
        terminator: 0x3a  # Colon ':'
      - id: value
        type: strz
        encoding: ascii
        terminator: 0x0d0a

    instances:
      is_empty:
        value: 'name == ""'

instances:
  content_length:
    value: 'headers.find { it.name.lowercase() == "content-length" }?.value.to_i'