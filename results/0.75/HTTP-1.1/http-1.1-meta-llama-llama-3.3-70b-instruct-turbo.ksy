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
      - id: path
        type: str
        delimiter: 32
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

  header:
    seq:
      - id: key
        type: str
        delimiter: 58
        encoding: ascii
      - id: colon
        type: str
        size: 1
        encoding: ascii
      - id: value
        type: str
        delimiter: 10
        encoding: ascii

  headers:
    seq:
      - id: entries
        type: header
        repeat: until
        until: { eq: { bytes: [13, 10, 13, 10] } }