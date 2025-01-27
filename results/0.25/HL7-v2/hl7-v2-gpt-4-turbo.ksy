meta:
  id: hl7_v2_gpt_4_turbo
  title: HL7 Version 2 Message
  file-extension: hl7
  endian: be
doc: |
  HL7 (Health Level Seven) is a set of international standards for the exchange, integration, sharing, and retrieval of electronic health information.

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
        repeat-until: _.is_terminator

  field:
    seq:
      - id: content
        type: str
        encoding: ASCII
        terminator: 0x7C # '|'
        consume: true
        include: false
    instances:
      is_terminator:
        value: content == '\r'