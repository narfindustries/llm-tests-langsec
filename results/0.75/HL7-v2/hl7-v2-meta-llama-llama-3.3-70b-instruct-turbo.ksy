meta:
  id: hl7-v2
  title: HL7 v2
  doc: Health Level Seven Version 2
seq:
  - id: message
    type: msg
types:
  msg:
    seq:
      - id: header
        type: seg
      - id: segments
        type: seg
        repeat: until
        repeat_until: seg_id == 'IEA'
      - id: trailer
        type: seg
  seg:
    seq:
      - id: seg_id
        type: str
        size: 3
      - id: fields
        type: field
        repeat: expr
        repeat_expr: len(value) > 0
      - id: repetition
        type: seg
        repeat: until
        repeat_until: seg_id != 'MSH'
  field:
    seq:
      - id: component
        type: str
        size: 1
      - id: repetition
        type: field
        repeat: expr
        repeat_expr: len(value) > 0