meta:
  id: http
  title: HTTP 1.1
  application: web
  file-extension: http
  endian: be

seq:
  - id: request
    type: request
    if: is_request

  - id: response
    type: response
    if: is_response

types:
  request:
    seq:
      - id: request_line
        type: request_line
      - id: headers
        type: headers
      - id: body
        size-eos: true
        if: has_body

  response:
    seq:
      - id: status_line
        type: status_line
      - id: headers
        type: headers
      - id: body
        size-eos: true
        if: has_body

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

  status_line:
    seq:
      - id: http_version
        type: strz
        encoding: ascii
        terminator: 0x20
      - id: status_code
        type: u2
      - id: reason_phrase
        type: strz
        encoding: ascii
        terminator: 0x0d0a

  headers:
    seq:
      - id: header_lines
        type: header_line
        repeat: until
        repeat-until: _.is_empty_line

  header_line:
    seq:
      - id: name
        type: strz
        encoding: ascii
        terminator: 0x3a
      - id: value
        type: strz
        encoding: ascii
        terminator: 0x0d0a

  body:
    seq:
      - id: content
        size-eos: true

instances:
  is_request:
    value: request_line.method != null

  is_response:
    value: status_line.http_version != null

  has_body:
    value: headers.header_lines.some { it.name.to_lower() == "content-length" or it.name.to_lower() == "transfer-encoding" }