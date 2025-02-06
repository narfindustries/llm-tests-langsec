meta:
  id: http
  title: HTTP 1.1
  application: http
  file-extension: http

seq:
  - id: request_line
    type: request_line
  - id: headers
    type: headers
  - id: body
    size-eos: true
    if: headers.content_length > 0

types:
  request_line:
    seq:
      - id: method
        type: strz
        encoding: ASCII
        terminator: 0x20
      - id: request_uri
        type: strz
        encoding: ASCII
        terminator: 0x20
      - id: http_version
        type: strz
        encoding: ASCII
        terminator: 0x0d0a

  headers:
    seq:
      - id: entries
        type: header
        repeat: eos

  header:
    seq:
      - id: name
        type: strz
        encoding: ASCII
        terminator: 0x3a
        include: false
      - id: value
        type: strz
        encoding: ASCII
        terminator: 0x0d0a
        include: false

    instances:
      content_length:
        value: '(name.trim().to_lower() == "content-length") ? value.trim().to_i : 0'
        if: name.trim().to_lower() == "content-length"

  response:
    seq:
      - id: status_line
        type: status_line
      - id: headers
        type: headers
      - id: body
        size-eos: true
        if: headers.content_length > 0

  status_line:
    seq:
      - id: http_version
        type: strz
        encoding: ASCII
        terminator: 0x20
      - id: status_code
        type: strz
        encoding: ASCII
        terminator: 0x20
      - id: reason_phrase
        type: strz
        encoding: ASCII
        terminator: 0x0d0a