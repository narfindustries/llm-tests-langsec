type: seq
- id: start_line
  type: seq
    - id: method
      type: str
      len: 0
    - id: space1
      type: str
      len: 1
      len_type: fixed
      contents: " "
    - id: uri
      type: str
      len: 0
    - id: space2
      type: str
      len: 1
      len_type: fixed
      contents: " "
    - id: version
      type: str
      len: 0
    - id: crlf1
      type: str
      len: 2
      len_type: fixed
      contents: "\r\n"
- id: headers
  type: seq
    - id: header_line
      type: seq
        - id: header_name
          type: str
          len: 0
        - id: colon
          type: str
          len: 1
          len_type: fixed
          contents: ":"
        - id: header_value
          type: str
          len: 0
        - id: crlf2
          type: str
          len: 2
          len_type: fixed
          contents: "\r\n"
- id: crlf3
  type: str
  len: 2
  len_type: fixed
  contents: "\r\n"
- id: body
  type: bytes
  len: -1 #This allows for a variable length body.  0 would mean no body.

