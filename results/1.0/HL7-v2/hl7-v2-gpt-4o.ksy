meta:
  id: hl7v2
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
      - id: segment_type
        type: strz
        terminator: 0x0D
      - id: fields
        type: field
        repeat: until
        repeat-until: _.is_end_of_segment

  field:
    seq:
      - id: value
        type: strz
        terminator: 0x7C

    instances:
      is_end_of_segment:
        value: value[value.size - 1] == '\r'