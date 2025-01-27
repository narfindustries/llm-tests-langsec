meta:
  id: hl7-v2
  title: HL7 Version 2
  endian: big
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
        until: seg.id == "IEA"
  seg:
    seq:
      - id: id
        type: str
        size: 3
      - id: fields
        type: field
        repeat: expr
        expr: fields != ""
  field:
    seq:
      - id: field
        type: str
        terminator: "~"