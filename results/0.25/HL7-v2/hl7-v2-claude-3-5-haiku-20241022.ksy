meta:
  id: hl7_v2
  file-extension: hl7
  endian: le
  encoding: ascii

seq:
  - id: message
    type: hl7_message

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
        encoding: ascii
      - id: fields
        type: field
        repeat: eos

  field:
    seq:
      - id: value
        type: strz
        encoding: ascii
        terminator: 0x7C
      - id: repetitions
        type: field
        repeat: expr
        repeat-expr: 0

  msh_segment:
    seq:
      - id: encoding_characters
        type: str
        size: 4
        encoding: ascii
      - id: sending_application
        type: strz
        encoding: ascii
      - id: sending_facility
        type: strz
        encoding: ascii
      - id: receiving_application
        type: strz
        encoding: ascii
      - id: receiving_facility
        type: strz
        encoding: ascii
      - id: message_datetime
        type: strz
        encoding: ascii
      - id: security
        type: strz
        encoding: ascii
        if: _io.pos < _io.size
      - id: message_type
        type: strz
        encoding: ascii
      - id: message_control_id
        type: strz
        encoding: ascii
      - id: processing_id
        type: strz
        encoding: ascii
      - id: version_id
        type: strz
        encoding: ascii

  pid_segment:
    seq:
      - id: patient_id
        type: strz
        encoding: ascii
      - id: patient_name
        type: strz
        encoding: ascii
      - id: mother_maiden_name
        type: strz
        encoding: ascii
        if: _io.pos < _io.size
      - id: date_of_birth
        type: strz
        encoding: ascii
      - id: sex
        type: strz
        encoding: ascii
      - id: patient_alias
        type: strz
        encoding: ascii
        if: _io.pos < _io.size
      - id: race
        type: strz
        encoding: ascii
        if: _io.pos < _io.size
      - id: patient_address
        type: strz
        encoding: ascii
      - id: country_code
        type: strz
        encoding: ascii
        if: _io.pos < _io.size

  pv1_segment:
    seq:
      - id: patient_class
        type: strz
        encoding: ascii
      - id: assigned_patient_location
        type: strz
        encoding: ascii
      - id: admission_type
        type: strz
        encoding: ascii
        if: _io.pos < _io.size
      - id: preadmit_number
        type: strz
        encoding: ascii
        if: _io.pos < _io.size
      - id: prior_patient_location
        type: strz
        encoding: ascii
        if: _io.pos < _io.size
      - id: attending_doctor
        type: strz
        encoding: ascii
        if: _io.pos < _io.size
      - id: referring_doctor
        type: strz
        encoding: ascii
        if: _io.pos < _io.size
      - id: consulting_doctor
        type: strz
        encoding: ascii
        if: _io.pos < _io.size

  obr_segment:
    seq:
      - id: set_id
        type: strz
        encoding: ascii
      - id: placer_order_number
        type: strz
        encoding: ascii
      - id: filler_order_number
        type: strz
        encoding: ascii
      - id: universal_service_id
        type: strz
        encoding: ascii
      - id: priority
        type: strz
        encoding: ascii
        if: _io.pos < _io.size
      - id: requested_datetime
        type: strz
        encoding: ascii
        if: _io.pos < _io.size
      - id: observation_datetime
        type: strz
        encoding: ascii
        if: _io.pos < _io.size
      - id: observation_end_datetime
        type: strz
        encoding: ascii
        if: _io.pos < _io.size

  obx_segment:
    seq:
      - id: set_id
        type: strz
        encoding: ascii
      - id: value_type
        type: strz
        encoding: ascii
      - id: observation_identifier
        type: strz
        encoding: ascii
      - id: observation_value
        type: strz
        encoding: ascii
      - id: units
        type: strz
        encoding: ascii
        if: _io.pos < _io.size
      - id: reference_range
        type: strz
        encoding: ascii
        if: _io.pos < _io.size
      - id: abnormal_flags
        type: strz
        encoding: ascii
        if: _io.pos < _io.size

enums:
  message_type:
    1: adt
    2: orm
    3: oru
    4: mdm
  patient_class:
    1: inpatient
    2: outpatient
    3: emergency
  sex:
    1: male
    2: female
    3: other