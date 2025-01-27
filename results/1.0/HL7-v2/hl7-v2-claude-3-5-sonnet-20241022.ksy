meta:
  id: hl7_v2
  file-extension: hl7
  endian: le
seq:
  - id: segments
    type: segment
    repeat: eos
types:
  segment:
    seq:
      - id: segment_type
        type: str
        size: 3
        encoding: ASCII
      - id: separator_1
        type: u1
      - id: fields
        type: field
        repeat: until
        repeat-until: _.terminator == 0x0d
  field:
    seq:
      - id: content
        type: str
        terminator: 0x7c
        encoding: ASCII
      - id: terminator
        type: u1
    instances:
      is_end:
        value: terminator == 0x0d