meta:
  id: http-1.1
  title: HTTP/1.1
  homepage: https://www.w3.org/Protocols/HTTP/1.1/
  file-extension: http
  endian: be

types:
  header-field:
    seq:
      - name: name
        type: str
      - name: value
        type: str

  request-line:
    seq:
      - name: method
        type: str
      - name: uri
        type: str
      - name: version
        type: str

  response-line:
    seq:
      - name: version
        type: str
      - name: status_code
        type: u2
      - name: reason_phrase
        type: str

  headers:
    seq:
      - name: header_fields
        type: header-field
        repeat: eos

  http-message:
    seq:
      - name: start_line
        type: switch:
          on: type
          cases:
            request: request-line
            response: response-line
      - name: headers
        type: headers
      - name: body
        type: bytes


  http-request:
    seq:
      - name: start_line
        type: request-line
      - name: headers
        type: headers
      - name: body
        type: bytes

  http-response:
    seq:
      - name: start_line
        type: response-line
      - name: headers
        type: headers
      - name: body
        type: bytes


instances:
  - id: http-request
    type: http-request
  - id: http-response
    type: http-response

