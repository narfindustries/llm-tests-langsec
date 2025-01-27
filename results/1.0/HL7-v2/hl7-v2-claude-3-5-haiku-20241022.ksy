meta:
  id: hl7_v2
  title: HL7 Version 2 Message Specification
  file-extension: hl7
  endian: le

seq:
  - id: header
    type: message_header
  - id: segments
    type: segment
    repeat: eos

types:
  message_header:
    seq:
      - id: field_separator
        type: str
        size: 1
      - id: encoding_characters 
        type: str
        size: 4
      - id: sending_application
        type: field
      - id: sending_facility
        type: field
      - id: receiving_application
        type: field
      - id: receiving_facility
        type: field
      - id: datetime
        type: field
      - id: security
        type: field
      - id: message_type
        type: field
      - id: message_control_id
        type: field
      - id: processing_id
        type: field
      - id: version
        type: field

  segment:
    seq:
      - id: name
        type: str
        size: 3
      - id: fields
        type: field
        repeat: eos

  field:
    seq:
      - id: components
        type: str
        terminator: 0x5E  # Field separator