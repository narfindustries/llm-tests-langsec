meta:
  id: hl7_v2
  title: Health Level 7 (HL7) Version 2
  file-extension: hl7
doc: |
  HL7 is a set of international standards for transfer of clinical and administrative data between software applications used by various healthcare providers.
seq:
  - id: segments
    type: segment
    repeat: eos
types:
  segment:
    seq:
      - id: field
        type: str
        encoding: ASCII
        terminator: 0x7C  # Pipe '|' character
      - id: field_repeat
        type: str
        terminator: 0x7e  # Tilde '~' character
    instances:
      segment_type:
        pos: 0
        size: 3
        encoding: ASCII