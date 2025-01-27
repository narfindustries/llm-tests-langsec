meta:
  id: http_message
  title: HTTP Message
  file-extension: http
  endian: be

seq:
  - id: start_line
    type: start_line
  - id: headers
    type: header
    repeat: until
    repeat-until: _.is_empty
  - id: body
    size-eos: true
    if: start_line.is_request == false

types:
  start_line:
    seq:
      - id: line
        type: strz
        encoding: utf-8
        terminator: 0x0a
    instances:
      is_request:
        value: line.starts_with('GET') or line.starts_with('POST') or line.starts_with('PUT') or line.starts_with('DELETE') or line.starts_with('HEAD') or line.starts_with('OPTIONS') or line.starts_with('PATCH')
      is_response:
        value: not is_request

  header:
    seq:
      - id: line
        type: strz
        encoding: utf-8
        terminator: 0x0a
    instances:
      is_empty:
        value: line == ""