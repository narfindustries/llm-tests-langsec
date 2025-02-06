meta:
  id: hl7_v2
  title: Health Level 7 (HL7) Version 2.x Message
  file-extension: hl7
  endian: le
doc: |
  HL7 V2.x messaging is used in the healthcare industry to communicate standard information
  between devices. These messages are highly structured with segments such as MSH (Message Header),
  PID (Patient Identification), etc.
types:
  message:
    seq:
      - id: segments
        type: segment
        repeat: eos

  segment:
    seq:
      - id: seg_id
        type: str
        encoding: ASCII
        size: 3
      - id: fields
        type: field
        repeat: until
        repeat-until: _.field_data == "|"

  field:
    seq:
      - id: field_data
        type: strz
        encoding: ASCII

  strz:
    seq:
      - id: string
        type: str
        encoding: ASCII
        terminator: 0x7c  # Field terminator '|'
    instances:
      eof:
        value: "_io.pos == _io.size"