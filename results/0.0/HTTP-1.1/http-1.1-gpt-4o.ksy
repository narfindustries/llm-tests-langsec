meta:
  id: http
  title: HTTP 1.1
  file-extension: http
  endian: be

seq:
  - id: request_line
    type: request_line
  - id: headers
    type: headers
  - id: body
    size-eos: true
    if: headers.has_body

types:
  request_line:
    seq:
      - id: method
        type: str
        encoding: ASCII
        terminator: 0x20
      - id: request_uri
        type: str
        encoding: ASCII
        terminator: 0x20
      - id: http_version
        type: str
        encoding: ASCII
        terminator: 0x0d0a

  headers:
    seq:
      - id: fields
        type: header_field
        repeat: until
        repeat-until: _.is_end_of_headers
      - id: end_of_headers
        type: str
        encoding: ASCII
        terminator: 0x0d0a
    instances:
      has_body:
        value: fields.exists { it.name == "Content-Length" } or fields.exists { it.name == "Transfer-Encoding" }

  header_field:
    seq:
      - id: name
        type: str
        encoding: ASCII
        terminator: 0x3a  # ':'
      - id: value
        type: str
        encoding: ASCII
        terminator: 0x0d0a
    instances:
      is_end_of_headers:
        value: name == "" and value == ""