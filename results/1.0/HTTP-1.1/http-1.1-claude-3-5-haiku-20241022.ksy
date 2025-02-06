meta:
  id: http1_1
  title: HTTP/1.1 Protocol
  file-extension: http
  endian: be
  encoding: ascii

seq:
  - id: request
    type: http_request
    repeat: expr
    repeat-expr: num_request

  - id: response
    type: http_response
    repeat: expr
    repeat-expr: num_response

types:
  http_request:
    seq:
      - id: method
        type: str
        encoding: ascii
        terminator: 0x20
        
      - id: uri
        type: str
        encoding: ascii
        terminator: 0x20
        
      - id: http_version
        type: str
        encoding: ascii
        terminator: 0x0D
        
      - id: headers
        type: header
        repeat: until
        repeat-until: _io.is_eof or _.name == ''
        
      - id: body
        type: body
        
    enums:
      http_methods:
        1: get
        2: post
        3: put
        4: delete
        5: head
        6: options
        7: trace
        8: connect
        
  header:
    seq:
      - id: name
        type: strz
        encoding: ascii
        
      - id: value
        type: strz
        encoding: ascii
        
  body:
    seq:
      - id: content_length
        type: u4
        
      - id: content
        type: str
        encoding: ascii
        size: content_length
        
  http_response:
    seq:
      - id: status_code
        type: u2
        enum: http_status_codes
        
      - id: status_message
        type: strz
        encoding: ascii
        
      - id: headers
        type: header
        repeat: until
        repeat-until: _io.is_eof or _.name == ''
        
      - id: body
        type: body
        
    enums:
      http_status_codes:
        # 1xx Informational
        100: continue
        101: switching_protocols
        
        # 2xx Successful
        200: ok
        201: created
        204: no_content
        
        # 3xx Redirection
        301: moved_permanently
        302: found
        304: not_modified
        
        # 4xx Client Error
        400: bad_request
        401: unauthorized
        403: forbidden
        404: not_found
        
        # 5xx Server Error
        500: internal_server_error
        501: not_implemented
        503: service_unavailable

instances:
  num_request:
    value: 1
    
  num_response:
    value: 1