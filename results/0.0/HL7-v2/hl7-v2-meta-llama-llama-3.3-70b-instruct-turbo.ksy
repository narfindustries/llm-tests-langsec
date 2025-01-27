meta:
  id: hl7-v2
  title: HL7 v2
  file-extension: hl7
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
        repeat: until-eof
  seg:
    seq:
      - id: name
        size: 3
      - id: fields
        type: field
        repeat: eos
      - id: trailer
        size: 1
  field:
    seq:
      - id: component
        type: component
        repeat: eos
  component:
    seq:
      - id: sub_component
        type: sub_component
        repeat: eos
    if: component != ''
  sub_component:
    seq:
      - id: value
        size: until-next