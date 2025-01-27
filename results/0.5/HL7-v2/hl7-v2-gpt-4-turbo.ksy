meta:
  id: hl7_v2
  title: HL7 Version 2 Message
  file-extension: hl7
  endian: be
  license: CC0-1.0
doc: |
  Health Level Seven (HL7) is a set of international standards for transfer of clinical and administrative data between software applications used by various healthcare providers.

seq:
  - id: segments
    type: segment
    repeat: eos

types:
  segment:
    seq:
      - id: segment_type
        type: str
        size: 3
        encoding: ASCII
    instances:
      message_header:
        pos: 0
        type: msh
        if: segment_type == "MSH"
      patient_identification:
        pos: 0
        type: pid
        if: segment_type == "PID"
      patient_visit:
        pos: 0
        type: pv1
        if: segment_type == "PV1"

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
        type: str
        size: 180
        encoding: ASCII
        terminator: 0x7C
      - id: sending_facility
        type: str
        size: 180
        encoding: ASCII
        terminator: 0x7C
      - id: receiving_application
        type: str
        size: 180
        encoding: ASCII
        terminator: 0x7C
      - id: receiving_facility
        type: str
        size: 180
        encoding: ASCII
        terminator: 0x7C
      - id: date_time_of_message
        type: str
        size: 26
        encoding: ASCII
        terminator: 0x7C
      - id: security
        type: str
        size: 40
        encoding: ASCII
        terminator: 0x7C
      - id: message_type
        type: str
        size: 7
        encoding: ASCII
        terminator: 0x7C
      - id: message_control_id
        type: str
        size: 199
        encoding: ASCII
        terminator: 0x7C
      - id: processing_id
        type: str
        size: 3
        encoding: ASCII
        terminator: 0x7C
      - id: version_id
        type: str
        size: 60
        encoding: ASCII
        terminator: 0x7C

  pid:
    seq:
      - id: set_id
        type: str
        size: 4
        encoding: ASCII
        terminator: 0x7C
      - id: patient_id
        type: str
        size: 20
        encoding: ASCII
        terminator: 0x7C
      - id: patient_identifier_list
        type: str
        size: 250
        encoding: ASCII
        terminator: 0x7C
      - id: alternate_patient_id
        type: str
        size: 20
        encoding: ASCII
        terminator: 0x7C
      - id: patient_name
        type: str
        size: 250
        encoding: ASCII
        terminator: 0x7C
      - id: mother_maiden_name
        type: str
        size: 250
        encoding: ASCII
        terminator: 0x7C
      - id: date_of_birth
        type: str
        size: 8
        encoding: ASCII
        terminator: 0x7C
      - id: sex
        type: str
        size: 1
        encoding: ASCII
        terminator: 0x7C
      - id: patient_address
        type: str
        size: 250
        encoding: ASCII
        terminator: 0x7C
      - id: phone_number_home
        type: str
        size: 40
        encoding: ASCII
        terminator: 0x7C
      - id: phone_number_business
        type: str
        size: 40
        encoding: ASCII
        terminator: 0x7C

  pv1:
    seq:
      - id: set_id
        type: str
        size: 4
        encoding: ASCII
        terminator: 0x7C
      - id: patient_class
        type: str
        size: 1
        encoding: ASCII
        terminator: 0x7C
      - id: assigned_patient_location
        type: str
        size: 80
        encoding: ASCII
        terminator: 0x7C
      - id: admission_type
        type: str
        size: 2
        encoding: ASCII
        terminator: 0x7C
      - id: preadmit_number
        type: str
        size: 20
        encoding: ASCII
        terminator: 0x7C
      - id: prior_patient_location
        type: str
        size: 80
        encoding: ASCII
        terminator: 0x7C
      - id: attending_doctor
        type: str
        size: 250
        encoding: ASCII
        terminator: 0x7C
      - id: referring_doctor
        type: str
        size: 250
        encoding: ASCII
        terminator: 0x7C
      - id: consulting_doctor
        type: str
        size: 250
        encoding: ASCII
        terminator: 0x7C
      - id: hospital_service
        type: str
        size: 3
        encoding: ASCII
        terminator: 0x7C
      - id: admission_date
        type: str
        size: 26
        encoding: ASCII
        terminator: 0x7C
      - id: discharge_date
        type: str
        size: 26
        encoding: ASCII
        terminator: 0x7C