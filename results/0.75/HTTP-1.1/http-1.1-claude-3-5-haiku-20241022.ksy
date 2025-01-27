meta:
  id: http_1_1
  title: HTTP/1.1 Protocol Specification
  file-extension: http
  endian: be

seq:
  - id: request
    type: request_message
    if: is_request

  - id: response
    type: response_message
    if: is_response

types:
  request_message:
    seq:
      - id: method
        type: str
        encoding: ASCII
        terminator: 0x20

      - id: uri
        type: str
        encoding: ASCII
        terminator: 0x20

      - id: http_version
        type: str
        encoding: ASCII
        terminator: 0x0D

      - id: headers
        type: header_block
        repeat: until
        repeat-until: _.is_last_header

  response_message:
    seq:
      - id: status_line
        type: status_line

      - id: headers
        type: header_block
        repeat: until
        repeat-until: _.is_last_header

      - id: body
        type: body

  status_line:
    seq:
      - id: http_version
        type: str
        encoding: ASCII
        terminator: 0x20

      - id: status_code
        type: str
        encoding: ASCII
        terminator: 0x20

      - id: status_text
        type: str
        encoding: ASCII
        terminator: 0x0D

  header_block:
    seq:
      - id: name
        type: str
        encoding: ASCII
        terminator: 0x3A

      - id: value
        type: str
        encoding: ASCII
        terminator: 0x0D

      - id: line_end
        contents: [0x0A]

      - id: is_last_header
        type: b1
        enum: header_termination

  body:
    seq:
      - id: content_length
        type: u4
        if: has_content_length

      - id: content
        type: str
        size: content_length
        encoding: ASCII
        if: has_content_length

enums:
  header_termination:
    0: continue
    1: end_of_headers

instances:
  is_request:
    value: method != ''

  is_response:
    value: status_line.status_code != ''

  has_content_length:
    value: headers.*.name.contains('Content-Length')