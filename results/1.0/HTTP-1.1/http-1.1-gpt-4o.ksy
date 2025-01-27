meta:
  id: http_1_1
  title: HTTP 1.1
  endian: be
  file-extension: http

seq:
  - id: request
    type: request

  - id: response
    type: response

types:
  request:
    seq:
      - id: method
        type: strz
        encoding: ascii

      - id: url
        type: strz
        encoding: ascii

      - id: version
        type: http_version

      - id: headers
        type: headers

      - id: body
        type: bytes
        size-eos: true

  response:
    seq:
      - id: version
        type: http_version

      - id: status_code
        type: u2
    
      - id: reason_phrase
        type: strz
        encoding: ascii

      - id: headers
        type: headers

      - id: body
        type: bytes
        size-eos: true

  http_version:
    seq:
      - id: http_marker
        contents: "HTTP/"

      - id: major
        type: u1
    
      - id: dot
        contents: "."

      - id: minor
        type: u1

  headers:
    seq:
      - id: header_items
        type: header
        repeat: until
        repeat-until: _io.eos || (hdr_terminator_exists and header_items[-1].is_terminator)

  header:
    seq:
      - id: name
        type: strz
        encoding: ascii
        terminator: 0x3A # colon ':'

      - id: value
        type: strz
        encoding: ascii
        terminator: 0x0D0A # CRLF

    instances:
      is_terminator:
        value: name == "" && value == "\r\n"