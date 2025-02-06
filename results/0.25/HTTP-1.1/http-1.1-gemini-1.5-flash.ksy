type: seq
  - id: start_line
    type: seq
      - id: method
        type: str
      - id: uri
        type: str
      - id: version
        type: str
  - id: headers
    type: seq
      - id: header
        type: seq
          - id: name
            type: str
          - id: value
            type: str
  - id: body
    type: bytes
