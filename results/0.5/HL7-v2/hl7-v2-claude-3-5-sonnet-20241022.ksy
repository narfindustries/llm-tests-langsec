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
      - id: segment_type
        type: str
        size: 3
        encoding: ASCII
      - id: fields
        type: field
        repeat: until
        repeat-until: _.field_content == "\r"
        
  field:
    seq:
      - id: field_separator
        type: str
        size: 1
        encoding: ASCII
      - id: field_content
        type: str
        encoding: ASCII
        size-eos: true
    
  msh_segment:
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
        size: 227
        encoding: ASCII
      - id: sending_facility
        type: str
        size: 227
        encoding: ASCII
      - id: receiving_application
        type: str
        size: 227
        encoding: ASCII
      - id: receiving_facility
        type: str
        size: 227
        encoding: ASCII
      - id: datetime
        type: str
        size: 26
        encoding: ASCII
      - id: security
        type: str
        size: 40
        encoding: ASCII
      - id: message_type
        type: str
        size: 15
        encoding: ASCII
      - id: message_control_id
        type: str
        size: 20
        encoding: ASCII
      - id: processing_id
        type: str
        size: 3
        encoding: ASCII
      - id: version_id
        type: str
        size: 60
        encoding: ASCII
      - id: sequence_number
        type: str
        size: 15
        encoding: ASCII
      - id: continuation_pointer
        type: str
        size: 180
        encoding: ASCII
      - id: accept_ack_type
        type: str
        size: 2
        encoding: ASCII
      - id: application_ack_type
        type: str
        size: 2
        encoding: ASCII
      - id: country_code
        type: str
        size: 3
        encoding: ASCII
      - id: character_set
        type: str
        size: 16
        encoding: ASCII
      - id: principal_language
        type: str
        size: 250
        encoding: ASCII

  pid_segment:
    seq:
      - id: set_id
        type: str
        size: 4
        encoding: ASCII
      - id: patient_id
        type: str
        size: 20
        encoding: ASCII
      - id: patient_identifier_list
        type: str
        size: 250
        encoding: ASCII
      - id: alternate_patient_id
        type: str
        size: 20
        encoding: ASCII
      - id: patient_name
        type: str
        size: 250
        encoding: ASCII
      - id: mothers_maiden_name
        type: str
        size: 250
        encoding: ASCII
      - id: date_of_birth
        type: str
        size: 26
        encoding: ASCII
      - id: sex
        type: str
        size: 1
        encoding: ASCII
      - id: patient_alias
        type: str
        size: 250
        encoding: ASCII
      - id: race
        type: str
        size: 250
        encoding: ASCII
      - id: patient_address
        type: str
        size: 250
        encoding: ASCII
      - id: county_code
        type: str
        size: 4
        encoding: ASCII
      - id: phone_number_home
        type: str
        size: 250
        encoding: ASCII
      - id: phone_number_business
        type: str
        size: 250
        encoding: ASCII
      - id: primary_language
        type: str
        size: 250
        encoding: ASCII
      - id: marital_status
        type: str
        size: 250
        encoding: ASCII
      - id: religion
        type: str
        size: 250
        encoding: ASCII
      - id: patient_account_number
        type: str
        size: 20
        encoding: ASCII
      - id: ssn
        type: str
        size: 16
        encoding: ASCII
      - id: drivers_license
        type: str
        size: 25
        encoding: ASCII

  obr_segment:
    seq:
      - id: set_id
        type: str
        size: 4
        encoding: ASCII
      - id: placer_order_number
        type: str
        size: 75
        encoding: ASCII
      - id: filler_order_number
        type: str
        size: 75
        encoding: ASCII
      - id: universal_service_id
        type: str
        size: 250
        encoding: ASCII
      - id: priority
        type: str
        size: 2
        encoding: ASCII
      - id: requested_datetime
        type: str
        size: 26
        encoding: ASCII
      - id: observation_datetime
        type: str
        size: 26
        encoding: ASCII
      - id: observation_end_datetime
        type: str
        size: 26
        encoding: ASCII
      - id: collection_volume
        type: str
        size: 20
        encoding: ASCII
      - id: collector_identifier
        type: str
        size: 250
        encoding: ASCII
      - id: specimen_action_code
        type: str
        size: 1
        encoding: ASCII
      - id: danger_code
        type: str
        size: 250
        encoding: ASCII

  obx_segment:
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
        type: str
        size: 250
        encoding: ASCII
      - id: observation_sub_id
        type: str
        size: 20
        encoding: ASCII
      - id: observation_value
        type: str
        size: 250
        encoding: ASCII
      - id: units
        type: str
        size: 250
        encoding: ASCII
      - id: references_range
        type: str
        size: 60
        encoding: ASCII
      - id: abnormal_flags
        type: str
        size: 5
        encoding: ASCII
      - id: probability
        type: str
        size: 5
        encoding: ASCII
      - id: nature_of_abnormal_test
        type: str
        size: 2
        encoding: ASCII
      - id: observation_result_status
        type: str
        size: 1
        encoding: ASCII
      - id: effective_date_last_normal
        type: str
        size: 26
        encoding: ASCII