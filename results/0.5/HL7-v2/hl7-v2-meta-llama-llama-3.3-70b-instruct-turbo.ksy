meta:
  id: hl7v2
  endian: le
seq:
  - id: segments
    type: segment
    repeat: until-after-eof
types:
  segment:
    seq:
      - id: segment_id
        type: str
        size: 3
        encoding: ascii
      - id: fields
        type: field
        repeat: eos
  field:
    seq:
      - id: field_value
        type: str
        size: until-next-delimiter
        delimiter: 124
        encoding: ascii
      - id: next_delimiter
        type: str
        size: 1
        encoding: ascii
        if: next_delimiter == "|"
  msh_segment:
    seq:
      - id: field_separator
        type: str
        size: 1
        encoding: ascii
      - id: encoding_characters
        type: str
        size: 4
        encoding: ascii
      - id: sending_application
        type: str
        size: 20
        encoding: ascii
      - id: sending_facility
        type: str
        size: 20
        encoding: ascii
      - id: receiving_application
        type: str
        size: 20
        encoding: ascii
      - id: receiving_facility
        type: str
        size: 20
        encoding: ascii
      - id: date_time_of_message
        type: str
        size: 26
        encoding: ascii
      - id: security
        type: str
        size: 40
        encoding: ascii
      - id: message_type
        type: str
        size: 3
        encoding: ascii
      - id: message_control_id
        type: str
        size: 20
        encoding: ascii
      - id: processing_id
        type: str
        size: 3
        encoding: ascii
      - id: version_id
        type: str
        size: 12
        encoding: ascii
      - id: sequence_number
        type: str
        size: 20
        encoding: ascii
      - id: continuation_pointer
        type: str
        size: 180
        encoding: ascii
      - id: accepting_application
        type: str
        size: 20
        encoding: ascii
      - id: accepting_facility
        type: str
        size: 20
        encoding: ascii
      - id: date_time_of_message_2
        type: str
        size: 26
        encoding: ascii
      - id: security_2
        type: str
        size: 40
        encoding: ascii
      - id: message_type_2
        type: str
        size: 3
        encoding: ascii
      - id: message_control_id_2
        type: str
        size: 20
        encoding: ascii
      - id: processing_id_2
        type: str
        size: 3
        encoding: ascii
      - id: version_id_2
        type: str
        size: 12
        encoding: ascii
      - id: sequence_number_2
        type: str
        size: 20
        encoding: ascii
      - id: continuation_pointer_2
        type: str
        size: 180
        encoding: ascii
  pid_segment:
    seq:
      - id: set_id
        type: str
        size: 4
        encoding: ascii
      - id: patient_id_external
        type: str
        size: 20
        encoding: ascii
      - id: patient_id_internal
        type: str
        size: 20
        encoding: ascii
      - id: patient_id_visit
        type: str
        size: 20
        encoding: ascii
      - id: patient_name
        type: str
        size: 80
        encoding: ascii
      - id: mother_maiden_name
        type: str
        size: 80
        encoding: ascii
      - id: date_of_birth
        type: str
        size: 8
        encoding: ascii
      - id: sex
        type: str
        size: 1
        encoding: ascii
      - id: patient_alias
        type: str
        size: 80
        encoding: ascii
      - id: race
        type: str
        size: 20
        encoding: ascii
      - id: patient_address
        type: str
        size: 80
        encoding: ascii
      - id: county_code
        type: str
        size: 20
        encoding: ascii
      - id: phone_number_home
        type: str
        size: 20
        encoding: ascii
      - id: phone_number_business
        type: str
        size: 20
        encoding: ascii
      - id: primary_language
        type: str
        size: 20
        encoding: ascii
      - id: marital_status
        type: str
        size: 1
        encoding: ascii
      - id: religion
        type: str
        size: 20
        encoding: ascii
      - id: patient_account_number
        type: str
        size: 20
        encoding: ascii
      - id: ssn
        type: str
        size: 20
        encoding: ascii
      - id: driver_s_license_number
        type: str
        size: 20
        encoding: ascii
      - id: mother_s_maiden_name
        type: str
        size: 80
        encoding: ascii
      - id: citizenship
        type: str
        size: 20
        encoding: ascii
      - id: death_date
        type: str
        size: 8
        encoding: ascii
      - id: death_cause
        type: str
        size: 80
        encoding: ascii
      - id: death_cause_code
        type: str
        size: 20
        encoding: ascii
      - id: death_location
        type: str
        size: 80
        encoding: ascii
      - id: death_location_code
        type: str
        size: 20
        encoding: ascii
      - id: autopsy
        type: str
        size: 1
        encoding: ascii
      - id: autopsy_result
        type: str
        size: 80
        encoding: ascii
      - id: organ_donor
        type: str
        size: 1
        encoding: ascii
  pv1_segment:
    seq:
      - id: set_id
        type: str
        size: 4
        encoding: ascii
      - id: visit_number
        type: str
        size: 20
        encoding: ascii
      - id: patient_class
        type: str
        size: 1
        encoding: ascii
      - id: visit_number_external
        type: str
        size: 20
        encoding: ascii
      - id: admission_type
        type: str
        size: 1
        encoding: ascii
      - id: admitting_doctor
        type: str
        size: 50
        encoding: ascii
      - id: visit_reason
        type: str
        size: 80
        encoding: ascii
      - id: consulting_doctor
        type: str
        size: 50
        encoding: ascii
      - id: hospital_service
        type: str
        size: 20
        encoding: ascii
      - id: admission_date
        type: str
        size: 8
        encoding: ascii
      - id: admission_time
        type: str
        size: 8
        encoding: ascii
      - id: discharge_date
        type: str
        size: 8
        encoding: ascii
      - id: discharge_time
        type: str
        size: 8
        encoding: ascii
      - id: visit_number_internal
        type: str
        size: 20
        encoding: ascii
      - id: alternate_visit_number
        type: str
        size: 20
        encoding: ascii
      - id: visit_indicator
        type: str
        size: 1
        encoding: ascii
      - id: patient_type
        type: str
        size: 20
        encoding: ascii
      - id: visit_description
        type: str
        size: 80
        encoding: ascii
      - id: admitting_location
        type: str
        size: 20
        encoding: ascii
      - id: discharge_location
        type: str
        size: 20
        encoding: ascii
      - id: discharge_disposition
        type: str
        size: 1
        encoding: ascii
      - id: discharge_diagnosis
        type: str
        size: 80
        encoding: ascii
      - id: discharge_diagnosis_code
        type: str
        size: 20
        encoding: ascii
      - id: visit_priority
        type: str
        size: 1
        encoding: ascii
      - id: pre_admit_number
        type: str
        size: 20
        encoding: ascii
      - id: prior_patient_location
        type: str
        size: 20
        encoding: ascii
      - id: admit_date_time
        type: str
        size: 26
        encoding: ascii
      - id: discharge_date_time
        type: str
        size: 26
        encoding: ascii
      - id: visit_number_unique
        type: str
        size: 20
        encoding: ascii
      - id: number_of_patients_in_room
        type: str
        size: 2
        encoding: ascii
      - id: visit_number_2
        type: str
        size: 20
        encoding: ascii
      - id: visit_indicator_2
        type: str
        size: 1
        encoding: ascii
  obr_segment:
    seq:
      - id: set_id
        type: str
        size: 4
        encoding: ascii
      - id: placer_order_number
        type: str
        size: 20
        encoding: ascii
      - id: filler_order_number
        type: str
        size: 20
        encoding: ascii
      - id: universal_service_identifier
        type: str
        size: 80
        encoding: ascii
      - id: priority
        type: str
        size: 1
        encoding: ascii
      - id: requested_date_time
        type: str
        size: 26
        encoding: ascii
      - id: observation_date_time
        type: str
        size: 26
        encoding: ascii
      - id: observation_end_date_time
        type: str
        size: 26
        encoding: ascii
      - id: collection_volume
        type: str
        size: 20
        encoding: ascii
      - id: collector_identifier
        type: str
        size: 20
        encoding: ascii
      - id: specimen_action_code
        type: str
        size: 1
        encoding: ascii
      - id: danger_code
        type: str
        size: 1
        encoding: ascii
      - id: relevant_clinical_information
        type: str
        size: 80
        encoding: ascii
      - id: specimen_received_date_time
        type: str
        size: 26
        encoding: ascii
      - id: specimen_source
        type: str
        size: 20
        encoding: ascii
      - id: ordering_provider
        type: str
        size: 50
        encoding: ascii
      - id: order_callback_phone_number
        type: str
        size: 20
        encoding: ascii
      - id: order_callback_phone_number_2
        type: str
        size: 20
        encoding: ascii
      - id: placer_field_1
        type: str
        size: 20
        encoding: ascii
      - id: placer_field_2
        type: str
        size: 20
        encoding: ascii
      - id: filler_field_1
        type: str
        size: 20
        encoding: ascii
      - id: filler_field_2
        type: str
        size: 20
        encoding: ascii
      - id: order_control
        type: str
        size: 2
        encoding: ascii
      - id: parent_universal_service_identifier
        type: str
        size: 80
        encoding: ascii
      - id: observation_result_status
        type: str
        size: 1
        encoding: ascii
      - id: effective_date_of_reference_ranges
        type: str
        size: 8
        encoding: ascii
      - id: user_defined_access_check
        type: str
        size: 20
        encoding: ascii
      - id: date_time_of_reference_ranges
        type: str
        size: 26
        encoding: ascii
      - id: number_of_sample_containers
        type: str
        size: 2
        encoding: ascii
      - id: laboratory_observation_instance_identifier
        type: str
        size: 20
        encoding: ascii
      - id: specimen_reserved
        type: str
        size: 1
        encoding: ascii
      - id: laboratory_equipment_instance_identifier
        type: str
        size: 20
        encoding: ascii
      - id: prenatal_test
        type: str
        size: 1
        encoding: ascii