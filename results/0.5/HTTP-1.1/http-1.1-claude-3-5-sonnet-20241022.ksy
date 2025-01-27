meta:
  id: http_1_1
  title: HTTP/1.1 Protocol
  file-extension: http
  endian: be

seq:
  - id: request_line
    type: request_line
  - id: headers
    type: header
    repeat: until
    repeat-until: _.is_empty
  - id: empty_line
    type: empty_line
  - id: body
    type: body
    if: has_body

types:
  request_line:
    seq:
      - id: method
        type: str
        terminator: 0x20
        encoding: ASCII
      - id: uri
        type: str
        terminator: 0x20
        encoding: ASCII
      - id: version
        type: str
        terminator: 0x0a
        encoding: ASCII

  header:
    seq:
      - id: name
        type: str
        terminator: 0x3a
        encoding: ASCII
      - id: space
        type: u1
      - id: value
        type: str
        terminator: 0x0a
        encoding: ASCII
    instances:
      is_empty:
        value: name.length == 0
      is_content_length:
        value: name.lower == "content-length"
      content_length:
        value: value.to_i if is_content_length

  empty_line:
    seq:
      - id: line_feed
        type: u1

  body:
    seq:
      - id: content
        size: content_size

instances:
  content_size:
    value: |
      headers.first(
        lambda x: x.is_content_length
      ).content_length
  has_body:
    value: content_size > 0