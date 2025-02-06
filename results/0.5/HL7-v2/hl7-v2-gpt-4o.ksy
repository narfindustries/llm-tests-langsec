meta:
  id: hl7_v2
  title: HL7 Version 2.x Message
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
        terminator: 0x0d
        encoding: ASCII
      - id: fields
        type: field
        repeat: eos

  field:
    seq:
      - id: value
        type: str
        terminator: 0x7c
        encoding: ASCII