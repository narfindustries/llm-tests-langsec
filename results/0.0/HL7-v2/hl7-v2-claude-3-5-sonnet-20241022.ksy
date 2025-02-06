meta:
  id: hl7_v2
  file-extension: hl7
  endian: le

seq:
  - id: segments
    type: segment
    repeat: eos

types:
  segment:
    seq:
      - id: segment_id
        type: str
        size: 3
        encoding: ASCII
      - id: field_separator
        type: u1
      - id: content
        type:
          switch-on: segment_id
          cases:
            '"MSH"': msh_segment
            '"PID"': pid_segment
            '"PV1"': pv1_segment
            _: unknown_segment

  msh_segment:
    seq:
      - id: encoding_characters
        type: str
        size: 4
        encoding: ASCII
      - id: sending_application
        type: hd_type
      - id: sending_facility
        type: hd_type
      - id: receiving_application
        type: hd_type
      - id: receiving_facility
        type: hd_type
      - id: datetime
        type: ts_type
      - id: security
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: message_type
        type: msg_type
      - id: message_control_id
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: processing_id
        type: pt_type
      - id: version_id
        type: str
        encoding: ASCII
        terminator: 0x7c

  pid_segment:
    seq:
      - id: set_id
        type: u1
      - id: patient_id
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: patient_identifier_list
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: alternate_patient_id
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: patient_name
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: mothers_maiden_name
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: datetime_of_birth
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: sex
        type: u1
        enum: sex_enum
      - id: patient_alias
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: race
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: patient_address
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: county_code
        type: str
        encoding: ASCII
        terminator: 0x7c

  pv1_segment:
    seq:
      - id: set_id
        type: u1
      - id: patient_class
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: assigned_patient_location
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: admission_type
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: preadmit_number
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: prior_patient_location
        type: str
        encoding: ASCII
        terminator: 0x7c

  hd_type:
    seq:
      - id: namespace_id
        type: str
        encoding: ASCII
        terminator: 0x5e
      - id: universal_id
        type: str
        encoding: ASCII
        terminator: 0x5e
      - id: universal_id_type
        type: str
        encoding: ASCII
        terminator: 0x7c

  ts_type:
    seq:
      - id: time
        type: str
        encoding: ASCII
        terminator: 0x7c

  msg_type:
    seq:
      - id: message_code
        type: str
        encoding: ASCII
        terminator: 0x5e
      - id: trigger_event
        type: str
        encoding: ASCII
        terminator: 0x5e
      - id: message_structure
        type: str
        encoding: ASCII
        terminator: 0x7c

  pt_type:
    seq:
      - id: processing_id
        type: str
        encoding: ASCII
        terminator: 0x5e
      - id: processing_mode
        type: str
        encoding: ASCII
        terminator: 0x7c

  unknown_segment:
    seq:
      - id: content
        size-eos: true

enums:
  sex_enum:
    70: female  # F
    77: male    # M
    79: other   # O
    85: unknown # U
    65: ambiguous # A