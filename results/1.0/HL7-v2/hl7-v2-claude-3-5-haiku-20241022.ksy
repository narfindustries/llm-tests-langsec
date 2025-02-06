meta:
  id: hl7_v2
  file-extension: hl7
  encoding: ascii
  endian: le

seq:
  - id: message
    type: hl7_message

types:
  hl7_message:
    seq:
      - id: msh_segment
        type: msh_segment
      - id: segments
        type: segment
        repeat: until
        repeat-until: _io.is_eof

  msh_segment:
    seq:
      - id: field_separator
        type: str
        encoding: ascii
        size: 1
      - id: encoding_characters
        type: str
        encoding: ascii
        size: 4
      - id: sending_application
        type: strz
        encoding: ascii
        terminator: 0x7C
      - id: sending_facility
        type: strz
        encoding: ascii
        terminator: 0x7C
      - id: receiving_application
        type: strz
        encoding: ascii
        terminator: 0x7C
      - id: receiving_facility
        type: strz
        encoding: ascii
        terminator: 0x7C
      - id: message_datetime
        type: strz
        encoding: ascii
        terminator: 0x7C
      - id: security
        type: strz
        encoding: ascii
        terminator: 0x7C
      - id: message_type
        type: strz
        encoding: ascii
        terminator: 0x7C
      - id: message_control_id
        type: strz
        encoding: ascii
        terminator: 0x7C
      - id: processing_id
        type: strz
        encoding: ascii
        terminator: 0x7C
      - id: version
        type: strz
        encoding: ascii
        terminator: 0x0D

  segment:
    seq:
      - id: segment_name
        type: str
        encoding: ascii
        size: 3
      - id: fields
        type: field
        repeat: until
        repeat-until: _io.is_eof or _buf[_io.pos] == 0x0D

  field:
    seq:
      - id: separator
        type: u1
        if: _io.pos > 0
      - id: value
        type: strz
        encoding: ascii
        terminator: 0x7C
        consume: false

enums:
  message_type:
    ADT: 1
    ORM: 2
    ORU: 3
    RDE: 4
    VXU: 5

  processing_mode:
    D: 1
    P: 2
    T: 3