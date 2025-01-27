meta:
  id: http_1_1
  title: HTTP/1.1
  license: MIT
  encoding: UTF-8

seq:
  - id: request_line
    type: request_line

  - id: headers
    type: header
    repeat: until
    until: seq[3].value == ""

  - id: body
    type: body

types:
  request_line:
    seq:
      - id: method
        type: str
        encoding: UTF-8
        terminator: ' '

      - id: path
        type: str
        encoding: UTF-8
        terminator: ' '

      - id: version
        type: str
        encoding: UTF-8
        terminator: '\r\n'

  header:
    seq:
      - id: name
        type: str
        encoding: UTF-8
        terminator: ': '

      - id: value
        type: str
        encoding: UTF-8
        terminator: '\r\n'

  body:
    seq:
      - id: length
        type: u4

      - id: payload
        type: str
        encoding: UTF-8
        size: length