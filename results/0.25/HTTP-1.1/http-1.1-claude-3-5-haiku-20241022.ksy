meta:
  id: http_1_1
  title: HTTP/1.1 Protocol Specification
  file-extension: http
  endian: be

seq:
  - id: request_or_response
    type:
      switch-on: _io.size
      cases:
        _: choice

types:
  choice:
    seq:
      - id: content
        type:
          switch-on: first_line_type
          cases:
            'request': request
            'response': response

  first_line_type:
    seq:
      - id: method
        type: str
        terminator: 0x20
        encoding: ASCII
        valid:
          any-of: 
            - 'GET'
            - 'POST'
            - 'PUT'
            - 'DELETE'
            - 'HEAD'
            - 'OPTIONS'
            - 'TRACE'
            - 'CONNECT'
            - 'PATCH'

  request:
    seq:
      - id: method
        type: str
        terminator: 0x20
        encoding: ASCII
      - id: uri
        type: str
        terminator: 0x20
        encoding: ASCII
      - id: http_version
        type: str
        terminator: 0x0D
        encoding: ASCII
      - id: headers
        type: header
      - id: body
        type: body
        size-eos: true

  response:
    seq:
      - id: status_line
        type: status_line
      - id: headers
        type: header
      - id: body
        type: body
        size-eos: true

  status_line:
    seq:
      - id: http_version
        type: str
        terminator: 0x20
        encoding: ASCII
      - id: status_code
        type: str
        terminator: 0x20
        encoding: ASCII
      - id: status_text
        type: str
        terminator: 0x0D
        encoding: ASCII

  header:
    seq:
      - id: headers
        type: header_field
        repeat: until
        repeat-until: not _io.eof or _.name.length == 0

  header_field:
    seq:
      - id: name
        type: str
        terminator: 0x3A
        encoding: ASCII
      - id: separator
        contents: [0x20]
      - id: value
        type: str
        terminator: 0x0D
        encoding: ASCII

  body:
    seq:
      - id: content
        type: str
        size-eos: true
        encoding: ASCII