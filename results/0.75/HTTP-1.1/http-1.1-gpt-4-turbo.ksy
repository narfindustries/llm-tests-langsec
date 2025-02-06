meta:
  id: http_1_1
  title: HTTP 1.1 Protocol
  file-extension: http
  endian: be
doc: |
  HTTP/1.1 is a revision of the HTTP protocol used for transferring
  hypertext pages across the Web. HTTP/1.1 improvements include
  extended methods, headers, and a new version of message parsing and
  transmission.

seq:
  - id: request_line
    type: request_line
  - id: headers
    type: headers
  - id: message_body
    size-eos: true

types:
  request_line:
    seq:
      - id: method
        type: strz
        encoding: ASCII
      - id: space1
        contents: " "
      - id: uri
        type: strz
        encoding: ASCII
      - id: space2
        contents: " "
      - id: http_version
        type: strz
        encoding: ASCII
      - id: crlf
        contents: [0x0D, 0x0A]

  headers:
    seq:
      - id: header_lines
        type: header_line
        repeat: eos

  header_line:
    seq:
      - id: line
        type: str
        terminator: 0x0A
    instances:
      parts:
        value: "line.split(':', maxsplit=1)"
      name:
        value: "parts[0].strip() if len(parts) > 0 else ''"
      value:
        value: "parts[1].strip() if len(parts) > 1 else ''"

  message_body:
    seq:
      - id: body
        type: u1
        repeat: eos
    doc: |
      The message body part of an HTTP message. It may contain binary
      data, hence it is treated as a raw byte array.