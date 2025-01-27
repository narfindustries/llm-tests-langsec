meta:
  id: http-1.1-gemini-1.5-flash
  title: HTTP/1.1, Gemini 1.5, and Flash
  homepage: ""
  authors: []
  license: ""

types:
  http_header:
    seq:
      - id: name
        type: str
      - id: value
        type: str

  http_request:
    seq:
      - id: method
        type: str
      - id: path
        type: str
      - id: version
        type: str
      - id: headers
        type: http_header*

  gemini_request:
    seq:
      - id: url
        type: str

  flash_header:
    seq:
      - id: size
        type: u4
      - id: data
        type: bytes

  flash_record:
    seq:
      - id: header
        type: flash_header
      - id: body
        type: bytes

  request:
    switch-on: type
    cases:
      http:
        type: http_request
      gemini:
        type: gemini_request
      flash:
        type: flash_record

  root:
    seq:
      - id: type
        type: u1
      - id: request
        type: request

