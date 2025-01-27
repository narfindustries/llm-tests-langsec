meta:
  id: http_1_1
  title: HTTP/1.1
  file-format: network-protocol
  endian: le
seq:
  - id: request_line
    type: request_line
  - id: headers
    type: headers
  - id: body
    type: body
types:
  request_line:
    seq:
      - id: method
        type: str
        size: 4
        encoding: ascii
      - id: space1
        type: str
        size: 1
        encoding: ascii
      - id: path
        type: str
        size: until_space
        encoding: ascii
      - id: space2
        type: str
        size: 1
        encoding: ascii
      - id: version
        type: str
        size: 8
        encoding: ascii
      - id: crlf
        type: str
        size: 2
        encoding: ascii
  headers:
    seq:
      - id: header
        type: header
        repeat: until_crlfcrlf
  header:
    seq:
      - id: name
        type: str
        size: until_colon
        encoding: ascii
      - id: colon
        type: str
        size: 1
        encoding: ascii
      - id: value
        type: str
        size: until_crlf
        encoding: ascii
      - id: crlf
        type: str
        size: 2
        encoding: ascii
  body:
    seq:
      - id: content
        type: str
        size: until_eof
        encoding: ascii