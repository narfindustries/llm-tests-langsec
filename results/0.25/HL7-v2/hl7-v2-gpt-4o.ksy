meta:
  id: hl7_v2
  title: HL7 v2 Message
  file-extension: hl7
  endian: be

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
        encoding: ascii
      - id: fields
        type: field
        repeat: eos

  field:
    seq:
      - id: value
        type: strz
        encoding: ascii
        terminator: 0x7C  # '|' character