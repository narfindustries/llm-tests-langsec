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
        encoding: ascii
        terminator: 0x20

      - id: uri
        type: str
        encoding: ascii
        terminator: 0x20

      - id: version
        type: str
        encoding: ascii
        terminator: 0x0d

      - id: headers
        type: header_section

      - id: body
        type: body_section
        size-eos: true

  response_message:
    seq:
      - id: version
        type: str
        encoding: ascii
        terminator: 0x20

      - id: status_code
        type: str
        encoding: ascii
        terminator: 0x20

      - id: status_text
        type: str
        encoding: ascii
        terminator: 0x0d

      - id: headers
        type: header_section

      - id: body
        type: body_section
        size-eos: true

  header_section:
    seq:
      - id: headers
        type: header
        repeat: until
        repeat-until: not _io.is_eof

  header:
    seq:
      - id: name
        type: str
        encoding: ascii
        terminator: 0x3a

      - id: value
        type: str
        encoding: ascii
        terminator: 0x0d

  body_section:
    seq:
      - id: content
        type: str
        encoding: ascii
        size-eos: true

instances:
  is_request:
    value: method != ''

  is_response:
    value: version.substring(0, 4) == 'HTTP'