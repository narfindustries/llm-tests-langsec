type: seq
- id: start_line
  type: obj
    - id: method
      type: str
    - id: uri
      type: str
    - id: http_version
      type: str
- id: headers
  type: seq
    - id: header
      type: obj
        - id: name
          type: str
        - id: value
          type: str
- id: body
  type: bytes
