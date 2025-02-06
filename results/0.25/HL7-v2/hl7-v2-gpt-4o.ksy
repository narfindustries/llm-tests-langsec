meta:
  id: hl7_v2
  title: HL7 v2.x Message
  file-extension: hl7
  encoding: ASCII

seq:
  - id: segments
    type: segment
    repeat: eos

types:
  segment:
    seq:
      - id: segment_id
        type: str
        size: 3
      - id: field_separator
        type: str
        size: 1
      - id: fields
        type: field
        repeat: until
        repeat-until: _io.peek_byte() == null

  field:
    seq:
      - id: value
        type: strz
        terminator: 124  # ASCII code for '|'
        include: true