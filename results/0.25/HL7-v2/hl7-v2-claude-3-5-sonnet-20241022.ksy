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
            '"PID"': pid_segment
            '"PV1"': pv1_segment
            '"OBR"': obr_segment
            '"OBX"': obx_segment
            _: unknown_segment

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
        type: hl7_string
      - id: sending_facility
        type: hl7_string
      - id: receiving_application
        type: hl7_string
      - id: receiving_facility
        type: hl7_string
      - id: datetime
        type: hl7_datetime
      - id: security
        type: hl7_string
      - id: message_type
        type: message_type
      - id: message_control_id
        type: hl7_string
      - id: processing_id
        type: processing_id
      - id: version_id
        type: hl7_string
      - id: sequence_number
        type: hl7_number
      - id: continuation_pointer
        type: hl7_string
      - id: accept_ack_type
        type: hl7_string
      - id: application_ack_type
        type: hl7_string
      - id: country_code
        type: hl7_string

  pid_segment:
    seq:
      - id: set_id
        type: hl7_number
      - id: patient_id
        type: hl7_string
      - id: patient_identifier_list
        type: hl7_string
      - id: alternate_patient_id
        type: hl7_string
      - id: patient_name
        type: hl7_string
      - id: mothers_maiden_name
        type: hl7_string
      - id: date_of_birth
        type: hl7_datetime
      - id: sex
        type: hl7_string
      - id: patient_alias
        type: hl7_string
      - id: race
        type: hl7_string
      - id: patient_address
        type: hl7_string
      - id: county_code
        type: hl7_string
      - id: phone_number_home
        type: hl7_string
      - id: phone_number_business
        type: hl7_string
      - id: primary_language
        type: hl7_string
      - id: marital_status
        type: hl7_string
      - id: religion
        type: hl7_string
      - id: patient_account_number
        type: hl7_string
      - id: ssn
        type: hl7_string
      - id: drivers_license
        type: hl7_string

  pv1_segment:
    seq:
      - id: set_id
        type: hl7_number
      - id: patient_class
        type: hl7_string
      - id: assigned_patient_location
        type: hl7_string
      - id: admission_type
        type: hl7_string
      - id: preadmit_number
        type: hl7_string
      - id: prior_patient_location
        type: hl7_string
      - id: attending_doctor
        type: hl7_string
      - id: referring_doctor
        type: hl7_string
      - id: consulting_doctor
        type: hl7_string
      - id: hospital_service
        type: hl7_string
      - id: temporary_location
        type: hl7_string
      - id: preadmit_test_indicator
        type: hl7_string
      - id: readmission_indicator
        type: hl7_string
      - id: admit_source
        type: hl7_string
      - id: ambulatory_status
        type: hl7_string
      - id: vip_indicator
        type: hl7_string
      - id: admitting_doctor
        type: hl7_string
      - id: patient_type
        type: hl7_string
      - id: visit_number
        type: hl7_string
      - id: financial_class
        type: hl7_string

  obr_segment:
    seq:
      - id: set_id
        type: hl7_number
      - id: placer_order_number
        type: hl7_string
      - id: filler_order_number
        type: hl7_string
      - id: universal_service_id
        type: hl7_string
      - id: priority
        type: hl7_string
      - id: requested_datetime
        type: hl7_datetime
      - id: observation_datetime
        type: hl7_datetime
      - id: observation_end_datetime
        type: hl7_datetime
      - id: collection_volume
        type: hl7_string
      - id: collector_identifier
        type: hl7_string
      - id: specimen_action_code
        type: hl7_string
      - id: danger_code
        type: hl7_string
      - id: relevant_clinical_info
        type: hl7_string
      - id: specimen_received_datetime
        type: hl7_datetime
      - id: specimen_source
        type: hl7_string
      - id: ordering_provider
        type: hl7_string
      - id: order_callback_phone_number
        type: hl7_string
      - id: placers_field_1
        type: hl7_string
      - id: placers_field_2
        type: hl7_string
      - id: filler_field_1
        type: hl7_string
      - id: filler_field_2
        type: hl7_string

  obx_segment:
    seq:
      - id: set_id
        type: hl7_number
      - id: value_type
        type: hl7_string
      - id: observation_identifier
        type: hl7_string
      - id: observation_sub_id
        type: hl7_string
      - id: observation_value
        type: hl7_string
      - id: units
        type: hl7_string
      - id: reference_range
        type: hl7_string
      - id: abnormal_flags
        type: hl7_string
      - id: probability
        type: hl7_string
      - id: nature_of_abnormal_test
        type: hl7_string
      - id: observation_result_status
        type: hl7_string
      - id: effective_date_last_normal_value
        type: hl7_datetime
      - id: user_defined_access_checks
        type: hl7_string
      - id: datetime_of_observation
        type: hl7_datetime
      - id: producers_id
        type: hl7_string
      - id: responsible_observer
        type: hl7_string
      - id: observation_method
        type: hl7_string

  message_type:
    seq:
      - id: message_code
        type: hl7_string
      - id: trigger_event
        type: hl7_string
      - id: message_structure
        type: hl7_string

  processing_id:
    seq:
      - id: processing_id
        type: str
        size: 1
        encoding: ASCII
      - id: processing_mode
        type: str
        size: 1
        encoding: ASCII

  hl7_string:
    seq:
      - id: value
        type: str
        encoding: ASCII
        terminator: 0x7C # |

  hl7_number:
    seq:
      - id: value
        type: str
        encoding: ASCII
        terminator: 0x7C # |

  hl7_datetime:
    seq:
      - id: value
        type: str
        encoding: ASCII
        terminator: 0x7C # |

  unknown_segment:
    seq:
      - id: content
        type: str
        encoding: ASCII
        size-eos: true