meta:
  id: http_1_1
  title: HTTP/1.1 Protocol Specification
  file-extension: 
    - http
    - txt
  license: MIT
  endian: be

seq:
  - id: request
    type: request_message
    if: not is_response

  - id: response  
    type: response_message
    if: is_response

types:
  request_message:
    seq:
      - id: method
        type: str
        encoding: ASCII
        terminator: 0x20
        
      - id: uri
        type: str
        encoding: ASCII
        terminator: 0x20
        
      - id: version
        type: str
        encoding: ASCII
        terminator: 0x0D
        
      - id: headers
        type: header
        repeat: eos
        
      - id: body
        type: body
        
  response_message:
    seq:
      - id: version
        type: str
        encoding: ASCII
        terminator: 0x20
        
      - id: status_code
        type: str
        encoding: ASCII
        terminator: 0x20
        
      - id: status_text
        type: str
        encoding: ASCII
        terminator: 0x0D
        
      - id: headers
        type: header
        repeat: eos
        
      - id: body
        type: body
        
  header:
    seq:
      - id: name
        type: str
        encoding: ASCII
        terminator: 0x3A
        
      - id: value
        type: str
        encoding: ASCII
        terminator: 0x0D
        
  body:
    seq:
      - id: content
        type: str
        encoding: ASCII
        size-eos: true

instances:
  is_response:
    value: 'true'
    doc: Determines if the message is a response