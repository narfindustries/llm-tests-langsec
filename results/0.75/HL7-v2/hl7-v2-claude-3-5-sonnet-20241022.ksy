meta:
  id: hl7v2
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
      - id: content
        type:
          switch-on: segment_type
          cases:
            '"MSH"': msh_segment
            '"EVN"': evn_segment
            '"PID"': pid_segment
            '"PV1"': pv1_segment
            '"OBR"': obr_segment
            '"OBX"': obx_segment
            _: default_segment

  default_segment:
    seq:
      - id: fields
        type: field
        repeat: eos

  field:
    seq:
      - id: components
        type: str
        encoding: ASCII
        terminator: 0x0d
        
  msh_segment:
    seq:
      - id: field_separator
        type: u1
      - id: encoding_characters
        type: str
        size: 4
        encoding: ASCII
      - id: sending_application
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: sending_facility
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: receiving_application
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: receiving_facility
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: datetime
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: security
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: message_type
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: message_control_id
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: processing_id
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: version_id
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: sequence_number
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: continuation_pointer
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: accept_ack_type
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: application_ack_type
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: country_code
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: character_set
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: principal_language
        type: str
        encoding: ASCII
        terminator: 0x0d

  evn_segment:
    seq:
      - id: event_type_code
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: recorded_datetime
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: planned_datetime
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: event_reason_code
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: operator_id
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: event_occurred
        type: str
        encoding: ASCII
        terminator: 0x0d

  pid_segment:
    seq:
      - id: set_id
        type: str
        encoding: ASCII
        terminator: 0x7c
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
      - id: administrative_sex
        type: str
        encoding: ASCII
        terminator: 0x7c
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
      - id: phone_number_home
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: phone_number_business
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: primary_language
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: marital_status
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: religion
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: patient_account_number
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: ssn_number
        type: str
        encoding: ASCII
        terminator: 0x0d

  pv1_segment:
    seq:
      - id: set_id
        type: str
        encoding: ASCII
        terminator: 0x7c
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
      - id: attending_doctor
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: referring_doctor
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: consulting_doctor
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: hospital_service
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: temporary_location
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: preadmit_test_indicator
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: readmission_indicator
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: admit_source
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: ambulatory_status
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: vip_indicator
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: admitting_doctor
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: patient_type
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: visit_number
        type: str
        encoding: ASCII
        terminator: 0x0d

  obr_segment:
    seq:
      - id: set_id
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: placer_order_number
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: filler_order_number
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: universal_service_id
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: priority
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: requested_datetime
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: observation_datetime
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: observation_end_datetime
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: collection_volume
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: collector_identifier
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: specimen_action_code
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: danger_code
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: relevant_clinical_info
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: specimen_received_datetime
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: specimen_source
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: ordering_provider
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: order_callback_phone_number
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: placers_field_1
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: placers_field_2
        type: str
        encoding: ASCII
        terminator: 0x0d

  obx_segment:
    seq:
      - id: set_id
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: value_type
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: observation_identifier
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: observation_sub_id
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: observation_value
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: units
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: references_range
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: abnormal_flags
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: probability
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: nature_of_abnormal_test
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: observation_result_status
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: effective_date_of_reference_range
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: user_defined_access_checks
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: datetime_of_the_observation
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: producers_id
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: responsible_observer
        type: str
        encoding: ASCII
        terminator: 0x7c
      - id: observation_method
        type: str
        encoding: ASCII
        terminator: 0x0d