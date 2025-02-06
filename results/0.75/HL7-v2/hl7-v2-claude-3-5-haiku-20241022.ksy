meta:
  id: hl7_v2
  file-extension: hl7
  encoding: ASCII
  endian: le

seq:
  - id: message
    type: hl7_message
    repeat: eos

types:
  hl7_message:
    seq:
      - id: segments
        type: segment
        repeat: eos

  segment:
    seq:
      - id: segment_name
        type: str
        size: 3
      - id: separator
        type: u1
        valid: 0x7C
      - id: fields
        type: field
        repeat: eos

  field:
    seq:
      - id: value
        type: str
        terminator: 0x7C
      - id: components
        type: component
        repeat: eos

  component:
    seq:
      - id: value
        type: str
        terminator: 0x5E
      - id: subcomponents
        type: subcomponent
        repeat: eos

  subcomponent:
    seq:
      - id: value
        type: str
        terminator: 0x26

enums:
  message_types:
    0x01: adt
    0x02: orm
    0x03: oru
    0x04: rde
    0x05: rgv
    0x06: mdm
    0x07: vxu
    0x08: siu

  trigger_events:
    0x01: admit_patient
    0x02: transfer_patient
    0x03: discharge_patient
    0x04: register_patient
    0x05: merge_patient
    0x06: order_message
    0x07: observation
    0x08: vaccination

  data_types:
    0x01: string
    0x02: numeric
    0x03: date
    0x04: time
    0x05: datetime
    0x06: composite_id
    0x07: coded_element
    0x08: person_name
    0x09: address