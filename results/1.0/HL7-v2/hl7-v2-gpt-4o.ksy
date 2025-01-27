meta:
  id: hl7_v2_message
  title: HL7 v2.x Message
  file-extension: hl7
  endian: be
doc: |
  HL7 v2.x is a widely used messaging standard in the healthcare domain that facilitates the exchange of clinical and administrative data.
seq:
  - id: message
    type: segment
    repeat: greedy
    
types:
  segment:
    seq:
      - id: header
        type: strz
        encoding: ascii
        terminator: 0x0d
      - id: fields
        type: field
        repeat: until
        repeat-until: _io.is_eof

  field:
    seq:
      - id: contents
        type: strz
        encoding: ascii
        terminator: [0x7c, 0x0d] # Field separator '|' or segment terminator CR

enums:
  segment_type:
    MSH: 0x4d5348
    PID: 0x504944
    # Add other segment types as needed