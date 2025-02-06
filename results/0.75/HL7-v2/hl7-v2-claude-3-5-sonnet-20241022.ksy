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
        type:
          switch-on: segment_type
          cases:
            '"MSH"': msh_segment
            '"EVN"': evn_segment
            '"PID"': pid_segment
            '"NK1"': nk1_segment
            '"PV1"': pv1_segment
            '"OBR"': obr_segment
            '"OBX"': obx_segment
            
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
        type: hl7_string
      - id: message_control_id
        type: hl7_string
      - id: processing_id
        type: hl7_string
      - id: version_id
        type: hl7_string

  pid_segment:
    seq:
      - id: set_id
        type: hl7_string
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
      - id: datetime_of_birth
        type: hl7_datetime
      - id: administrative_sex
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
      - id: ssn_number
        type: hl7_string

  evn_segment:
    seq:
      - id: event_type_code
        type: hl7_string
      - id: recorded_datetime
        type: hl7_datetime
      - id: planned_event_datetime
        type: hl7_datetime
      - id: event_reason_code
        type: hl7_string
      - id: operator_id
        type: hl7_string
      - id: event_occurred
        type: hl7_datetime

  pv1_segment:
    seq:
      - id: set_id
        type: hl7_string
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
      - id: admission_datetime
        type: hl7_datetime
      - id: discharge_datetime
        type: hl7_datetime

  obr_segment:
    seq:
      - id: set_id
        type: hl7_string
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

  obx_segment:
    seq:
      - id: set_id
        type: hl7_string
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
      - id: references_range
        type: hl7_string
      - id: abnormal_flags
        type: hl7_string
      - id: probability
        type: hl7_string
      - id: nature_of_abnormal_test
        type: hl7_string
      - id: observation_result_status
        type: hl7_string
      - id: effective_date
        type: hl7_datetime

  nk1_segment:
    seq:
      - id: set_id
        type: hl7_string
      - id: name
        type: hl7_string
      - id: relationship
        type: hl7_string
      - id: address
        type: hl7_string
      - id: phone_number
        type: hl7_string
      - id: business_phone_number
        type: hl7_string
      - id: contact_role
        type: hl7_string
      - id: start_date
        type: hl7_datetime
      - id: end_date
        type: hl7_datetime
      - id: next_of_kin_job_title
        type: hl7_string
      - id: next_of_kin_job_code
        type: hl7_string
      - id: next_of_kin_employee_number
        type: hl7_string

  hl7_string:
    seq:
      - id: value
        type: str
        size-eos: true
        encoding: ASCII

  hl7_datetime:
    seq:
      - id: value
        type: str
        size: 14
        encoding: ASCII