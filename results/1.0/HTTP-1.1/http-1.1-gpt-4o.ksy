meta:
  id: http_message
  title: HTTP 1.1 Message
  file-extension: http
  endian: be

seq:
  - id: start_line
    type: start_line
  - id: headers
    type: header_entries
  - id: body
    size: headers.content_length
    if: headers.is_content_length_defined

types:
  start_line:
    seq:
      - id: method
        type: strz
        encoding: ascii
        if: _parent.is_request
      - id: request_uri
        type: strz
        encoding: ascii
        if: _parent.is_request
      - id: http_version
        type: strz
        encoding: ascii
      - id: status_code
        type: strz
        encoding: ascii
        if: _parent.is_response
      - id: reason_phrase
        type: strz
        encoding: ascii
        if: _parent.is_response

  header_entries:
    seq:
      - id: entries
        type: header_entry
        repeat: until
        repeat-until: entries[_index].is_end_of_headers

    instances:
      content_length:
        value: entries.filter { it.name.to_lower() == "content-length" }[0].value.to_i
      is_content_length_defined:
        value: entries.any { it.name.to_lower() == "content-length" }

  header_entry:
    seq:
      - id: name
        type: strz
        encoding: ascii
        terminator: 0x3A # ':' character
      - id: value
        type: strz
        encoding: ascii
        terminator: 0x0A0D # CR LF

    instances:
      is_end_of_headers:
        value: name == "" and value == ""

instances:
  is_request:
    value: start_line.method != null
  is_response:
    value: start_line.status_code != null