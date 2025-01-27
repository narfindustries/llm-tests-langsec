meta:
  id: hl7_v2
  title: HL7 v2.x
  file-extension: hl7
  encoding: utf-8

doc: |
  HL7 version 2.x is a widely-used messaging standard for healthcare information exchange.
  This Kaitai Struct specification attempts to parse basic HL7 v2.x messages.

seq:
  - id: segments
    type: segment
    repeat: until
    repeat-until: _.is_end_of_message

types:
  segment:
    seq:
      - id: segment_id
        size: 3
        type: str
        doc: "3-character segment identifier"
      - id: fields
        type: field
        repeat: until
        repeat-until: _.is_end_of_segment

  field:
    seq:
      - id: value
        type: str
        terminator: 0x7C  # '|'
        doc: "Field value, terminated by pipe character"

    instances:
      is_end_of_segment:
        value: _parent.segment_id == "MSH" && value == "\r"
        doc: "Detects end of a segment within a message"

instances:
  is_end_of_message:
    value: segments[-1].segment_id == "MSH" && segments[-1].fields[-1].value == "\r"
    doc: "Detects end of the entire HL7 message"
