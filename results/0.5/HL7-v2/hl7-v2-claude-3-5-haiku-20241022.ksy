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
      - id: msh_segment
        type: msh_segment
      - id: segments
        type: segment
        repeat: eos

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
        type: str
        encoding: ascii
        size-eos: true
      - id: sending_facility
        type: str
        encoding: ascii
        size-eos: true
      - id: receiving_application
        type: str
        encoding: ascii
        size-eos: true
      - id: receiving_facility
        type: str
        encoding: ascii
        size-eos: true
      - id: timestamp
        type: str
        encoding: ascii
        size-eos: true
      - id: security
        type: str
        encoding: ascii
        size-eos: true
      - id: message_type
        type: str
        encoding: ascii
        size-eos: true
      - id: message_control_id
        type: str
        encoding: ascii
        size-eos: true
      - id: processing_id
        type: str
        encoding: ascii
        size-eos: true
      - id: version
        type: str
        encoding: ascii
        size-eos: true

  segment:
    seq:
      - id: pid_segment
        type: pid_segment
        if: _io.pos < _io.size
      - id: pv1_segment
        type: pv1_segment
        if: _io.pos < _io.size
      - id: obr_segment
        type: obr_segment
        if: _io.pos < _io.size
      - id: obx_segment
        type: obx_segment
        if: _io.pos < _io.size

  pid_segment:
    seq:
      - id: set_id
        type: str
        encoding: ascii
        size-eos: true
      - id: patient_id
        type: str
        encoding: ascii
        size-eos: true
      - id: patient_name
        type: str
        encoding: ascii
        size-eos: true
      - id: mother_maiden_name
        type: str
        encoding: ascii
        size-eos: true
      - id: date_of_birth
        type: str
        encoding: ascii
        size-eos: true
      - id: sex
        type: str
        encoding: ascii
        size-eos: true
      - id: patient_race
        type: str
        encoding: ascii
        size-eos: true

  pv1_segment:
    seq:
      - id: set_id
        type: str
        encoding: ascii
        size-eos: true
      - id: patient_class
        type: str
        encoding: ascii
        size-eos: true
      - id: assigned_patient_location
        type: str
        encoding: ascii
        size-eos: true
      - id: admission_type
        type: str
        encoding: ascii
        size-eos: true
      - id: preadmit_number
        type: str
        encoding: ascii
        size-eos: true

  obr_segment:
    seq:
      - id: set_id
        type: str
        encoding: ascii
        size-eos: true
      - id: placer_order_number
        type: str
        encoding: ascii
        size-eos: true
      - id: filler_order_number
        type: str
        encoding: ascii
        size-eos: true
      - id: universal_service_identifier
        type: str
        encoding: ascii
        size-eos: true
      - id: observation_date_time
        type: str
        encoding: ascii
        size-eos: true

  obx_segment:
    seq:
      - id: set_id
        type: str
        encoding: ascii
        size-eos: true
      - id: value_type
        type: str
        encoding: ascii
        size-eos: true
      - id: observation_identifier
        type: str
        encoding: ascii
        size-eos: true
      - id: observation_value
        type: str
        encoding: ascii
        size-eos: true
      - id: units
        type: str
        encoding: ascii
        size-eos: true