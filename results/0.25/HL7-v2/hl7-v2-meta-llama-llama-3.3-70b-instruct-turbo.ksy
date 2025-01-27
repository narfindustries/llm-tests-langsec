meta:
  id: hl7-v2
  title: Health Level 7 Version 2
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
        repeat: until
        until: is Terminator
  seg:
    seq:
      - id: code
        type: s
        size: 3
      - id: fields
        type: f
        repeat: until
        until: is Terminator
  f:
    seq:
      - id: value
        type: s
        size: -1
      - id: repetition
        type: f
        repeat: until
        until: is Terminator
  s:
    seq:
      - id: value
        type: str
        size: -1
        terminator: '\r'