meta:
  id: http1_1
  title: HTTP/1.1 Protocol
  file-extension: http
  encoding: utf-8
seq:
  - id: request_line
    type: request_line
  - id: headers
    type: headers
  - id: body
    size-eos: true
types:
  request_line:
    seq:
      - id: method
        type: strz
        encoding: ascii
        terminator: 0x20  # Space (' ')
      - id: request_uri
        type: strz
        encoding: ascii
        terminator: 0x20  # Space (' ')
      - id: http_version
        type: strz
        encoding: ascii
        terminator: 0x0d0a  # CRLF

  headers:
    seq:
      - id: entries
        type: header
        repeat: until
        repeat-until: entries_terminator

  header:
    seq:
      - id: name
        type: strz
        encoding: ascii
        terminator: 0x3a  # Colon (':')
      - id: ws
        type: str
        size: 1
        encoding: ascii
      - id: value
        type: strz
        encoding: ascii
        terminator: 0x0d0a  # CRLF

  entries_terminator:
    seq:
      - id: empty_line
        type: strz
        encoding: ascii
        terminator: 0x0d0a  # CRLF
        contents: ""