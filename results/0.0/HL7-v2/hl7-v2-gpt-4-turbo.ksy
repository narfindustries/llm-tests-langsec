meta:
  id: hl7_v2
  title: HL7 Version 2 Message
  file-extension: hl7
  endian: be
  license: CC0-1.0
  ks-version: 0.9

doc: |
  Health Level 7 (HL7) is a set of international standards for transfer of clinical and administrative data between software applications used by various healthcare providers.

seq:
  - id: segments
    type: segment
    repeat: eos

types:
  segment:
    seq:
      - id: segment_type
        type: str
        encoding: ASCII
        size: 3
      - id: fields
        type: field
        repeat: until
        repeat-until: _.is_last_field

  field:
    seq:
      - id: field_content
        type: str
        encoding: ASCII
        terminator: 0x7C # '|'
        consume: true
        include: false
    instances:
      is_last_field:
        value: field_content == '\r'