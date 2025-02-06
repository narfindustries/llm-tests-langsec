seq:
  - id: msh
    type: msh_segment

types:
  msh_segment:
    seq:
      - id: field_separator
        type: str
        size: 1
      - id: encoding_characters
        type: str
        size: 1
      - id: sending_facility
        type: string
      - id: sending_facility_namespace_id
        type: string
      - id: receiving_facility
        type: string
      - id: receiving_facility_namespace_id
        type: string
      - id: date_time_of_message
        type: date_time
      - id: security
        type: string
      - id: message_type
        type: string
      - id: message_control_id
        type: string
      - id: processing_id
        type: string
      - id: version_id
        type: string
      - id: sequence_number
        type: int
      - id: continuation_pointer
        type: string
      - id: accept_acknowledgment_type
        type: string
      - id: application_acknowledgment_type
        type: string
      - id: country_code
        type: string
        size: 2
      - id: character_set
        type: string
      - id: principal_language_of_message
        type: string
        size: 2

  pid_segment:
    seq:
      - id: set_id_patient_id
        type: int
      - id: patient_id
        type: string
      - id: patient_id_list
        type: pid_patient_id_list
      - id: alternate_patient_id
        type: string
      - id: patient_name
        type: string
      - id: mother_s_maiden_name
        type: string
      - id: date_of_birth
        type: date
      - id: sex
        type: string
        enum: "M,F,U"
      - id: patient_alias
        type: string
      - id: race
        type: string
      - id: patient_gender
        type: string
        enum: "M,F,U"
      - id: birth_place
        type: string
      - id: death_date
        type: date
      - id: death_time
        type: time
      - id: pronunciation
        type: string
      - id: species_code
        type: string
      - id: breed_code
        type: string
      - id: strain
        type: string
      - id: production_class_code
        type: string

  pid_patient_id_list:
    seq:
      - id: patient_id_list_item
        type: string

  pv1_segment:
    seq:
      - id: set_id_patient_visit
        type: int
      - id: visit_number
        type: string
      - id: patient_class
        type: string
        enum: "I,O,E"
      - id: visit_number_hospital
        type: string
      - id: admitting_doctor
        type: string
      - id: financial_class
        type: string
      - id: pre_admit_number
        type: string
      - id: prior_patient_location
        type: string
      - id: admission_date_time
        type: date_time
      - id: discharge_date_time
        type: date_time
      - id: visit_indicator
        type: string
        enum: "NEW,RETURN"
      - id: other_healthcare_provider
        type: string
      - id: visit_number_facility
        type: string
      - id: visit_number_universal
        type: string
      - id: bed_status
        type: string
        enum: "OCC,EMP"

  orc_segment:
    seq:
      - id: order_control
        type: string
        enum: "NW,CA"
      - id: placer_order_number
        type: string
      - id: filler_order_number
        type: string
      - id: placer_group_number
        type: string
      - id: order_status
        type: string
        enum: "SC,IP"
      - id: response_flag
        type: string
        enum: "R,N"
      - id: quantity_timing
        type: string
      - id: parent_order
        type: string
      - id: date_time_of_transaction
        type: date_time
      - id: entered_by
        type: string
      - id: verified_by
        type: string
      - id: parent_result
        type: string

  obr_segment:
    seq:
      - id: set_id_observation_request
        type: int
      - id: placer_order_number
        type: string
      - id: filler_order_number
        type: string
      - id: universal_service_id
        type: string
      - id: priority
        type: string
        enum: "STAT,ROUTINE"
      - id: requested_date_time
        type: date_time
      - id: observation_date_time
        type: date_time
      - id: observation_result_status
        type: string
        enum: "P,F"
      - id: effective_date_time_of_reference_range
        type: date_time
      - id: user_defined_access_checks
        type: string
      - id: observation_result_duration
        type: string
      - id: observation_result_status_modifier
        type: string
      - id: other_service_major_code
        type: string
      - id: other_service_minor_code
        type: string
      - id: other_service_modifier
        type: string

  obx_segment:
    seq:
      - id: set_id_observation_result
        type: int
      - id: value_type
        type: string
        enum: "NM,TX"
      - id: observation_result
        type: string
      - id: units
        type: string
      - id: references_range
        type: string
      - id: abnormal_flags
        type: string
        enum: "H,L"
      - id: probability
        type: string
      - id: nature_of_abnormal_test
        type: string
        enum: "A,N"
      - id: result_state
        type: string
        enum: "P,F"
      - id: result_status_modifier
        type: string
      - id: parent_result
        type: string
      - id: observation_result_date_time_of_the_observation
        type: date_time
      - id: observation_result_producer_s_id
        type: string
      - id: observation_result_responder_s_id
        type: string

  date_time:
    seq:
      - id: date
        type: date
      - id: time
        type: time

  date:
    seq:
      - id: year
        type: int
        size: 4
      - id: month
        type: int
        size: 2
      - id: day
        type: int
        size: 2

  time:
    seq:
      - id: hour
        type: int
        size: 2
      - id: minute
        type: int
        size: 2
      - id: second
        type: int
        size: 2

  string:
    seq:
      - id: value
        type: str
        size: 0 
        encoding: ascii 

meta:
  encoding: ascii