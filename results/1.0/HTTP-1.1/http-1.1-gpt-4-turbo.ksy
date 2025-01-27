meta:
  id: http
  title: HTTP 1.1 Protocol
  application: HTTP
  xref:
    rfc: 7230

seq:
  - id: request_line
    type: line
  - id: headers
    type: header
    repeat: eos
  - id: message_body
    size-eos: true

types:
  line:
    seq:
      - id: method
        type: str
        encoding: ASCII
        terminator: 0x20
      - id: request_target
        type: str
        encoding: ASCII
        terminator: 0x20
      - id: http_version
        type: str
        encoding: ASCII
        terminator: 0x0D
      - id: crlf
        contents: [0x0D, 0x0A]

  header:
    seq:
      - id: name
        type: str
        encoding: ASCII
        terminator: 0x3A  # colon
      - id: space
        contents: [0x20] # optional whitespace
      - id: value
        type: str
        encoding: ASCII
        terminator: 0x0D
      - id: crlf
        contents: [0x0D, 0x0A]

  str:
    seq:
      - id: value
        type: strz
        encoding: ASCII
        terminator: 0x20
