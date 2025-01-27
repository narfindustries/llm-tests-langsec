meta:
  id: http
  title: HTTP
  endian: le

seq:
  - id: request_line
    type: request_line

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
      - id: url
        type: str
        size: 100
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
      - id: name
        type: str
        size: 20
        encoding: ascii
      - id: colon
        type: str
        size: 1
        encoding: ascii
      - id: space
        type: str
        size: 1
        encoding: ascii
      - id: value
        type: str
        size: 100
        encoding: ascii
      - id: crlf
        type: str
        size: 2
        encoding: ascii

  body:
    seq:
      - id: content
        type: str
        size: 1000
        encoding: ascii