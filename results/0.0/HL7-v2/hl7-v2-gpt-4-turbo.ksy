meta:
  id: hl7_v2
  title: HL7 Version 2.x Message
  file-extension: hl7
  endian: be
doc: |
  HL7 Version 2.x messages are used in healthcare to exchange, integrate, share, and retrieve electronic health information. This specification covers the basic structure of HL7 v2.x messages, including common segments and fields.
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
      - id: fields
        type: field
        repeat: eos
        terminator: 0x0D # Carriage Return (CR) as segment terminator

  field:
    seq:
      - id: components
        type: str
        encoding: ASCII
        repeat: eos
        terminator: "|"

  msh_segment:
    seq:
      - id: field_separator
        type: str
        size: 1
      - id: encoding_characters
        type: str
        size: 4
      - id: sending_application
        type: str
        terminator: "|"
      - id: sending_facility
        type: str
        terminator: "|"
      - id: receiving_application
        type: str
        terminator: "|"
      - id: receiving_facility
        type: str
        terminator: "|"
      - id: date_time_of_message
        type: str
        terminator: "|"
      - id: security
        type: str
        terminator: "|"
      - id: message_type
        type: str
        terminator: "|"
      - id: message_control_id
        type: str
        terminator: "|"
      - id: processing_id
        type: str
        terminator: "|"
      - id: version_id
        type: str
        terminator: "|"
      - id: sequence_number
        type: str
        terminator: "|"
      - id: continuation_pointer
        type: str
        terminator: "|"
      - id: accept_acknowledgment_type
        type: str
        terminator: "|"
      - id: application_acknowledgment_type
        type: str
        terminator: "|"
      - id: country_code
        type: str
        terminator: "|"
      - id: character_set
        type: str
        terminator: "|"
      - id: principal_language_of_message
        type: str
        terminator: "|"
      - id: alternate_character_set_handling_scheme
        type: str
        terminator: "|"
      - id: message_profile_identifier
        type: str
        terminator: "|"
        repeat: eos
        terminator: 0x0D

  pid_segment:
    seq:
      - id: set_id_pid
        type: str
        terminator: "|"
      - id: patient_id
        type: str
        terminator: "|"
      - id: patient_identifier_list
        type: str
        terminator: "|"
      - id: alternate_patient_id_pid
        type: str
        terminator: "|"
      - id: patient_name
        type: str
        terminator: "|"
      - id: mother_s_maiden_name
        type: str
        terminator: "|"
      - id: date_time_of_birth
        type: str
        terminator: "|"
      - id: administrative_sex
        type: str
        terminator: "|"
      - id: patient_alias
        type: str
        terminator: "|"
      - id: race
        type: str
        terminator: "|"
      - id: patient_address
        type: str
        terminator: "|"
      - id: county_code
        type: str
        terminator: "|"
      - id: phone_number_home
        type: str
        terminator: "|"
      - id: phone_number_business
        type: str
        terminator: "|"
      - id: primary_language
        type: str
        terminator: "|"
      - id: marital_status
        type: str
        terminator: "|"
      - id: religion
        type: str
        terminator: "|"
      - id: patient_account_number
        type: str
        terminator: "|"
      - id: ssn_number_patient
        type: str
        terminator: "|"
      - id: driver_s_license_number_patient
        type: str
        terminator: "|"
      - id: mother_s_identifier
        type: str
        terminator: "|"
      - id: ethnic_group
        type: str
        terminator: "|"
      - id: birth_place
        type: str
        terminator: "|"
      - id: multiple_birth_indicator
        type: str
        terminator: "|"
      - id: birth_order
        type: str
        terminator: "|"
      - id: citizenship
        type: str
        terminator: "|"
      - id: veterans_military_status
        type: str
        terminator: "|"
      - id: nationality
        type: str
        terminator: "|"
      - id: patient_death_date_and_time
        type: str
        terminator: "|"
      - id: patient_death_indicator
        type: str
        terminator: "|"
      - id: identity_unknown_indicator
        type: str
        terminator: "|"
      - id: identity_reliability_code
        type: str
        terminator: "|"
      - id: last_update_date_time
        type: str
        terminator: "|"
      - id: last_update_facility
        type: str
        terminator: "|"
      - id: species_code
        type: str
        terminator: "|"
      - id: breed_code
        type: str
        terminator: "|"
      - id: strain
        type: str
        terminator: "|"
      - id: production_class_code
        type: str
        terminator: "|"
      - id: tribal_citizenship
        type: str
        terminator: "|"
        repeat: eos
        terminator: 0x0D