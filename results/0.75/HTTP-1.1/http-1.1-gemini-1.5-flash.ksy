types:
  http_message:
    seq:
      - id: start_line
        type: start_line
      - id: headers
        type: headers
      - id: body
        type: body

  start_line:
    seq:
      - id: method
        type: str
      - id: uri
        type: str
      - id: version
        type: str

  headers:
    type: array
    elements: header_field
    

  header_field:
    seq:
      - id: header_name
        type: str
      - id: header_value
        type: str

  body:
    id: body_data
    type: bytes

