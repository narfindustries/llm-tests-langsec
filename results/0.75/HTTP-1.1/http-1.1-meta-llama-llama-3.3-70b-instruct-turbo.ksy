meta:
  id: http_1_1
  endian: le

seq:
  - id: request_line
    type: request_line
  - id: headers
    type: header
    repeat: until
    until: eq(seq(-1).value, "\r\n\r\n")
  - id: body
    type: body
    if: request_line.method not in ["HEAD", "CONNECT"]

types:
  request_line:
    seq:
      - id: method
        type: str
        encoding: ascii
        enum: ["GET", "HEAD", "POST", "PUT", "DELETE", "CONNECT", "OPTIONS", "TRACE"]
      - id: sp1
        type: str
        encoding: ascii
        len: 1
        contents: " "
      - id: request_uri
        type: str
        encoding: ascii
      - id: sp2
        type: str
        encoding: ascii
        len: 1
        contents: " "
      - id: http_version
        type: str
        encoding: ascii
        regex: "^HTTP/1\\.1$"

  header:
    seq:
      - id: name
        type: str
        encoding: ascii
        terminator: "\r\n"
      - id: value
        type: str
        encoding: ascii
        terminator: "\r\n"

  body:
    seq:
      - id: content
        type: switch
        cases:
          - condition: headers["transfer-encoding"] == "chunked"
            type: chunked_body
          - condition: headers["content-length"] != null
            type: fixed_length_body
          - condition: headers["content-type"] == "multipart/form-data"
            type: multipart_body
          - type: entity_body

  chunked_body:
    seq:
      - id: chunks
        type: chunk
        repeat: until
        until: eq(this.length, 0)

  chunk:
    seq:
      - id: length
        type: hex
        enum: [0-9a-fA-F]
      - id: sp1
        type: str
        encoding: ascii
        len: 2
        contents: "\r\n"
      - id: data
        type: str
        encoding: ascii
        len: length
      - id: sp2
        type: str
        encoding: ascii
        len: 2
        contents: "\r\n"

  fixed_length_body:
    seq:
      - id: content
        type: str
        encoding: ascii
        len: headers["content-length"]

  multipart_body:
    seq:
      - id: parts
        type: part
        repeat: until
        until: eq(this.length, 0)

  part:
    seq:
      - id: headers
        type: header
        repeat: until
        until: eq(seq(-1).value, "\r\n\r\n")
      - id: body
        type: str
        encoding: ascii
        len: headers["content-length"]

  entity_body:
    seq:
      - id: content
        type: str
        encoding: ascii

response:
  seq:
    - id: status_line
      type: status_line
    - id: response_headers
      type: response_header
      repeat: until
      until: eq(seq(-1).value, "\r\n\r\n")
    - id: response_body
      type: response_body_type
      if: status_line.code not in ["204", "304", "205"]

types:
  status_line:
    seq:
      - id: http_version
        type: str
        encoding: ascii
        regex: "^HTTP/1\\.1$"
      - id: sp1
        type: str
        encoding: ascii
        len: 1
        contents: " "
      - id: code
        type: str
        encoding: ascii
        enum: ["100", "101", "200", "201", "202", "203", "204", "205", "206", "300", "301", "302", "303", "304", "305", "307", "400", "401", "402", "403", "404", "405", "406", "407", "408", "409", "410", "411", "412", "413", "414", "415", "416", "417", "500", "501", "502", "503", "504", "505"]
      - id: sp2
        type: str
        encoding: ascii
        len: 1
        contents: " "
      - id: reason_phrase
        type: str
        encoding: ascii

  response_header:
    seq:
      - id: name
        type: str
        encoding: ascii
        terminator: "\r\n"
      - id: value
        type: str
        encoding: ascii
        terminator: "\r\n"

  response_body_type:
    seq:
      - id: content
        type: switch
        cases:
          - condition: response_headers["transfer-encoding"] == "chunked"
            type: chunked_body
          - condition: response_headers["content-length"] != null
            type: fixed_length_body
          - condition: response_headers["content-type"] == "multipart/form-data"
            type: multipart_body
          - type: entity_body