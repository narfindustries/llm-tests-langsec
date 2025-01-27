meta:
  id: dns-gemini-1
  title: DNS Gemini 1.5 Flash
  license: Apache-2.0
types:
  - id: header
    struct:
      - id: magic
        type: u4
      - id: version
        type: u2
      - id: header_size
        type: u2
      - id: data_size
        type: u4
  - id: entry
    seq:
      - id: type
        type: u2
      - id: length
        type: u2
      - id: data
        type: bytes
        size: length
  - id: data_section
    seq:
      - id: entries
        type: entry
        repeat: expr
        repeat-expr: header.data_size / entry.length
