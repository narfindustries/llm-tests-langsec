meta:
  id: http
  title: HTTP
  license: mit
  author: [your name]
  doc: |
    HTTP (Hypertext Transfer Protocol) is a protocol for transferring
    data over the web.

seq:
  - id: request_line
    type: str
    encoding: ascii
    terminator: \r\n
  - id: headers
    type: seq
    repeat: eos
    seq:
      - id: header_name
        type: str
        encoding: ascii
        terminator: :
      - id: header_value
        type: str
        encoding: ascii
        terminator: \r\n
      - id: header_terminator
        type: str
        encoding: ascii
        value: \r\n
  - id: body
    type: str
    encoding: ascii
    size: eos