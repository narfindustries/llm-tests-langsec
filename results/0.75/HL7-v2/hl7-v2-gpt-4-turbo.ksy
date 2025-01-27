meta:
  id: hl7_v2
  title: Health Level 7 (HL7) Version 2
  file-extension: hl7
  endian: be
  license: CC0-1.0
  ks-version: 0.9

doc: |
  HL7 Version 2.x is a specification for interchange of healthcare data between systems.

seq:
  - id: segments
    type: segment
    repeat: eos

types:
  segment:
    seq:
      - id: seg_type
        type: str
        size: 3
        encoding: ASCII
    instances:
      msh_segment:
        pos: 0
        type: msh
        if: seg_type == "MSH"
      pid_segment:
        pos: 0
        type: pid
        if: seg_type == "PID"
      orc_segment:
        pos: 0
        type: orc
        if: seg_type == "ORC"
      obx_segment:
        pos: 0
        type: obx
        if: seg_type == "OBX"

  msh:
    seq:
      - id: field_separator
        type: str
        size: 1
        encoding: ASCII
      - id: encoding_characters
        type: str
        size: 4
        encoding: ASCII
      - id: sending_application
        size: 0
        type: strz
        encoding: ASCII
        terminator: 0x7c
      - id: sending_facility
        size: 0
        type: strz
        encoding: ASCII
        terminator: 0x7c
      - id: receiving_application
        size: 0
        type: strz
        encoding: ASCII
        terminator: 0x7c
      - id: receiving_facility
        size: 0
        type: strz
        encoding: ASCII
        terminator: 0x7c
      - id: datetime_of_message
        type: str
        size: 14
        encoding: ASCII
      - id: security
        size: 0
        type: strz
        encoding: ASCII
        terminator: 0x7c
      - id: message_type
        type: str
        size: 7
        encoding: ASCII
      - id: message_control_id
        size: 0
        type: strz
        encoding: ASCII
        terminator: 0x7c
      - id: processing_id
        type: str
        size: 3
        encoding: ASCII
      - id: version_id
        type: str
        size: 5
        encoding: ASCII

  pid:
    seq:
      - id: set_id
        type: str
        size: 4
        encoding: ASCII
      - id: patient_id
        size: 0
        type: strz
        encoding: ASCII
        terminator: 0x7c
      - id: patient_name
        type: str
        size: 250
        encoding: ASCII
      - id: date_of_birth
        type: str
        size: 8
        encoding: ASCII
      - id: sex
        type: str
        size: 1
        encoding: ASCII
      - id: patient_address
        size: 0
        type: strz
        encoding: ASCII
        terminator: 0x7c
      - id: phone_number_home
        size: 0
        type: strz
        encoding: ASCII
        terminator: 0x7c
      - id: phone_number_business
        size: 0
        type: strz
        encoding: ASCII
        terminator: 0x7c

  orc:
    seq:
      - id: order_control
        type: str
        size: 2
        encoding: ASCII
      - id: placer_order_number
        size: 0
        type: strz
        encoding: ASCII
        terminator: 0x7c

  obx:
    seq:
      - id: set_id
        type: str
        size: 4
        encoding: ASCII
      - id: value_type
        type: str
        size: 2
        encoding: ASCII
      - id: observation_identifier
        size: 0
        type: strz
        encoding: ASCII
        terminator: 0x7c
      - id: observation_value
        type: str
        size: 250
        encoding: ASCII
      - id: units
        size: 0
        type: strz
        encoding: ASCII
        terminator: 0x7c
      - id: references_range
        size: 0
        type: strz
        encoding: ASCII
        terminator: 0x7c