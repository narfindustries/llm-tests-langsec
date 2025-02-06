meta:
  id: hl7v2
  title: HL7 Version 2.x Message Specification
  file-extension: hl7
  encoding: UTF-8
  endian: le
  license: MIT
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
      - id: fields
        type: field
        repeat: until
        repeat-until: _io.eof
  field:
    seq:
      - id: field_value
        type: str
        size-eos: true
    params:
      - id: delimiter
        type: str
  component:
    seq:
      - id: component_value
        type: str
        size-eos: true
    params:
      - id: delimiter
        type: str
  subcomponent:
    seq:
      - id: subcomponent_value
        type: str
        size-eos: true
    params:
      - id: delimiter
        type: str
  msh:
    seq:
      - id: field_separator
        type: str
        size: 1
      - id: encoding_characters
        type: str
        size: 4
      - id: sending_application
        type: str
        size-eos: true
      - id: sending_facility
        type: str
        size-eos: true
      - id: receiving_application
        type: str
        size-eos: true
      - id: receiving_facility
        type: str
        size-eos: true
      - id: date_time_of_message
        type: str
        size-eos: true
      - id: security
        type: str
        size-eos: true
      - id: message_type
        type: str
        size-eos: true
      - id: message_control_id
        type: str
        size-eos: true
      - id: processing_id
        type: str
        size-eos: true
      - id: version_id
        type: str
        size-eos: true
      - id: sequence_number
        type: str
        size-eos: true
      - id: continuation_pointer
        type: str
        size-eos: true
      - id: accept_acknowledgement_type
        type: str
        size-eos: true
      - id: application_acknowledgement_type
        type: str
        size-eos: true
      - id: country_code
        type: str
        size-eos: true
      - id: character_set
        type: str
        size-eos: true
      - id: principal_language_of_message
        type: str
        size-eos: true
  pid:
    seq:
      - id: set_id
        type: str
        size-eos: true
      - id: patient_id
        type: str
        size-eos: true
      - id: patient_identifier_list
        type: str
        size-eos: true
      - id: alternate_patient_id
        type: str
        size-eos: true
      - id: patient_name
        type: str
        size-eos: true
      - id: mother_s_maiden_name
        type: str
        size-eos: true
      - id: date_time_of_birth
        type: str
        size-eos: true
      - id: sex
        type: str
        size-eos: true
      - id: patient_alias
        type: str
        size-eos: true
      - id: race
        type: str
        size-eos: true
      - id: patient_address
        type: str
        size-eos: true
      - id: county_code
        type: str
        size-eos: true
      - id: phone_number_home
        type: str
        size-eos: true
      - id: phone_number_business
        type: str
        size-eos: true
      - id: primary_language
        type: str
        size-eos: true
      - id: marital_status
        type: str
        size-eos: true
      - id: religion
        type: str
        size-eos: true
      - id: patient_account_number
        type: str
        size-eos: true
      - id: ssn_number_patient
        type: str
        size-eos: true
      - id: driver_s_license_number_patient
        type: str
        size-eos: true
      - id: mother_s_identifier
        type: str
        size-eos: true
      - id: ethnic_group
        type: str
        size-eos: true
      - id: birth_place
        type: str
        size-eos: true
      - id: multiple_birth_indicator
        type: str
        size-eos: true
      - id: birth_order
        type: str
        size-eos: true
      - id: citizenship
        type: str
        size-eos: true
      - id: veterans_military_status
        type: str
        size-eos: true
      - id: nationality
        type: str
        size-eos: true
      - id: patient_death_date_and_time
        type: str
        size-eos: true
      - id: patient_death_indicator
        type: str
        size-eos: true
      - id: identity_reliability_code
        type: str
        size-eos: true