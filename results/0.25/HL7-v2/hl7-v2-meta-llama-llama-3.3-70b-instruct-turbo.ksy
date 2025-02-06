meta:
  id: hl7v2
  endian: le

seq:
  - id: msh
    type: msh_segment

types:
  msh_segment:
    seq:
      - id: field_separator
        type: str
        len: 1
        encoding: ascii
      - id: encoding_characters
        type: str
        len: 3
        encoding: ascii
      - id: sending_application
        type: str
        len: 20
        encoding: ascii
      - id: sending_facility
        type: str
        len: 20
        encoding: ascii
      - id: receiving_application
        type: str
        len: 20
        encoding: ascii
      - id: receiving_facility
        type: str
        len: 20
        encoding: ascii
      - id: date_time_of_message
        type: str
        len: 14
        encoding: ascii
      - id: security
        type: str
        len: 40
        encoding: ascii
      - id: message_type
        type: str
        len: 3
        encoding: ascii
      - id: message_control_id
        type: str
        len: 20
        encoding: ascii
      - id: processing_id
        type: str
        len: 1
        encoding: ascii
      - id: version_id
        type: str
        len: 5
        encoding: ascii
      - id: sequence_number
        type: str
        len: 20
        encoding: ascii
      - id: continuation_pointer
        type: str
        len: 180
        encoding: ascii
      - id: accept_acknowledgment_type
        type: str
        len: 2
        encoding: ascii
      - id: application_acknowledgment_type
        type: str
        len: 2
        encoding: ascii
      - id: country_code
        type: str
        len: 3
        encoding: ascii
      - id: character_set
        type: str
        len: 10
        encoding: ascii
      - id: principal_language_of_message
        type: str
        len: 3
        encoding: ascii
    doc: Message Header Segment

  pid_segment:
    seq:
      - id: set_id
        type: str
        len: 4
        encoding: ascii
      - id: patient_id
        type: str
        len: 20
        encoding: ascii
      - id: patient_id_internal
        type: str
        len: 20
        encoding: ascii
      - id: alternate_patient_id
        type: str
        len: 20
        encoding: ascii
      - id: patient_name
        type: str
        len: 50
        encoding: ascii
      - id: mother_s_maiden_name
        type: str
        len: 20
        encoding: ascii
      - id: date_of_birth
        type: str
        len: 8
        encoding: ascii
      - id: sex
        type: str
        len: 1
        encoding: ascii
      - id: patient_alias
        type: str
        len: 20
        encoding: ascii
      - id: race
        type: str
        len: 10
        encoding: ascii
      - id: patient_address
        type: str
        len: 50
        encoding: ascii
      - id: county_code
        type: str
        len: 3
        encoding: ascii
      - id: phone_number_home
        type: str
        len: 20
        encoding: ascii
      - id: phone_number_business
        type: str
        len: 20
        encoding: ascii
      - id: citizenship
        type: str
        len: 10
        encoding: ascii
      - id: death_date
        type: str
        len: 8
        encoding: ascii
      - id: death_time
        type: str
        len: 6
        encoding: ascii
      - id: death_cause
        type: str
        len: 20
        encoding: ascii
      - id: death_cause_code
        type: str
        len: 10
        encoding: ascii
      - id: patient_death_indicator
        type: str
        len: 1
        encoding: ascii
    doc: Patient Identification Segment

  pv1_segment:
    seq:
      - id: set_id
        type: str
        len: 4
        encoding: ascii
      - id: visit_number
        type: str
        len: 20
        encoding: ascii
      - id: patient_class
        type: str
        len: 2
        encoding: ascii
      - id: visit_number_internal
        type: str
        len: 20
        encoding: ascii
      - id: financial_class
        type: str
        len: 10
        encoding: ascii
      - id: pre_admit_number
        type: str
        len: 20
        encoding: ascii
      - id: prior_patient_location
        type: str
        len: 10
        encoding: ascii
      - id: admitting_doctor
        type: str
        len: 20
        encoding: ascii
      - id: referring_doctor
        type: str
        len: 20
        encoding: ascii
      - id: discharge_disposition
        type: str
        len: 2
        encoding: ascii
      - id: discharge_disposition_code
        type: str
        len: 10
        encoding: ascii
      - id: discharge_to_location
        type: str
        len: 10
        encoding: ascii
      - id: diet_type
        type: str
        len: 10
        encoding: ascii
      - id: diet_supplement
        type: str
        len: 10
        encoding: ascii
      - id: special_accommodations
        type: str
        len: 10
        encoding: ascii
      - id: isolation
        type: str
        len: 10
        encoding: ascii
      - id: hospital_service
        type: str
        len: 10
        encoding: ascii
      - id: admit_source
        type: str
        len: 2
        encoding: ascii
      - id: ambulatory_status
        type: str
        len: 2
        encoding: ascii
      - id: visit_priority
        type: str
        len: 2
        encoding: ascii
      - id: pre_admit_test_indicator
        type: str
        len: 1
        encoding: ascii
      - id: readmission_indicator
        type: str
        len: 1
        encoding: ascii
      - id: patient_type
        type: str
        len: 2
        encoding: ascii
    doc: Patient Visit Segment

  orc_segment:
    seq:
      - id: order_control
        type: str
        len: 2
        encoding: ascii
      - id: placer_order_number
        type: str
        len: 20
        encoding: ascii
      - id: filler_order_number
        type: str
        len: 20
        encoding: ascii
      - id: order_status
        type: str
        len: 2
        encoding: ascii
      - id: response_flag
        type: str
        len: 1
        encoding: ascii
      - id: quantity_timing
        type: str
        len: 20
        encoding: ascii
      - id: parent_order
        type: str
        len: 20
        encoding: ascii
      - id: date_time_of_transaction
        type: str
        len: 14
        encoding: ascii
      - id: entered_by
        type: str
        len: 20
        encoding: ascii
      - id: verified_by
        type: str
        len: 20
        encoding: ascii
      - id: ordering_provider
        type: str
        len: 20
        encoding: ascii
      - id: enterer_callback_phone_number
        type: str
        len: 20
        encoding: ascii
      - id: order_effective_date_time
        type: str
        len: 14
        encoding: ascii
      - id: order_control_code_reason
        type: str
        len: 10
        encoding: ascii
      - id: entering_organization
        type: str
        len: 10
        encoding: ascii
      - id: entering_device
        type: str
        len: 10
        encoding: ascii
      - id: action_by_code
        type: str
        len: 10
        encoding: ascii
      - id: advanced_beneficiary_notice_code
        type: str
        len: 10
        encoding: ascii
      - id: ordering_facility_name
        type: str
        len: 20
        encoding: ascii
      - id: ordering_facility_address
        type: str
        len: 50
        encoding: ascii
      - id: ordering_facility_phone_number
        type: str
        len: 20
        encoding: ascii
      - id: ordering_facility_id
        type: str
        len: 20
        encoding: ascii
      - id: ordering_provider_address
        type: str
        len: 50
        encoding: ascii
      - id: order_status_modifier
        type: str
        len: 10
        encoding: ascii
      - id: advanced_beneficiary_notice_override_reason
        type: str
        len: 10
        encoding: ascii
      - id: filler_expected_specimen_volume
        type: str
        len: 10
        encoding: ascii
      - id: filler_obtained_specimen_volume
        type: str
        len: 10
        encoding: ascii
      - id: specimen_availability
        type: str
        len: 10
        encoding: ascii
      - id: specimen_rejection_reason
        type: str
        len: 10
        encoding: ascii
      - id: specimen_collection_method
        type: str
        len: 10
        encoding: ascii
      - id: specimen_source_site
        type: str
        len: 10
        encoding: ascii
      - id: specimen_site_modifier
        type: str
        len: 10
        encoding: ascii
    doc: Order Segment

  obr_segment:
    seq:
      - id: set_id
        type: str
        len: 4
        encoding: ascii
      - id: placer_order_number
        type: str
        len: 20
        encoding: ascii
      - id: filler_order_number
        type: str
        len: 20
        encoding: ascii
      - id: universal_service_id
        type: str
        len: 20
        encoding: ascii
      - id: priority
        type: str
        len: 2
        encoding: ascii
      - id: requested_date_time
        type: str
        len: 14
        encoding: ascii
      - id: observation_date_time
        type: str
        len: 14
        encoding: ascii
      - id: observation_result_status
        type: str
        len: 1
        encoding: ascii
      - id: who_subject_definition
        type: str
        len: 10
        encoding: ascii
      - id: observation_type
        type: str
        len: 10
        encoding: ascii
      - id: specimen_required
        type: str
        len: 1
        encoding: ascii
      - id: specimen_source
        type: str
        len: 10
        encoding: ascii
      - id: specimen_site
        type: str
        len: 10
        encoding: ascii
      - id: specimen_collection_site
        type: str
        len: 10
        encoding: ascii
      - id: specimen_source_site_modifier
        type: str
        len: 10
        encoding: ascii
      - id: specimen_availability
        type: str
        len: 10
        encoding: ascii
      - id: specimen_rejection_reason
        type: str
        len: 10
        encoding: ascii
      - id: specimen_collection_method
        type: str
        len: 10
        encoding: ascii
      - id: specimen_type
        type: str
        len: 10
        encoding: ascii
      - id: specimen_additives
        type: str
        len: 10
        encoding: ascii
      - id: specimen_special_handling
        type: str
        len: 10
        encoding: ascii
      - id: specimen_collection_volume
        type: str
        len: 10
        encoding: ascii
      - id: specimen_received_volume
        type: str
        len: 10
        encoding: ascii
      - id: specimen_received_date_time
        type: str
        len: 14
        encoding: ascii
      - id: specimen_expiration_date_time
        type: str
        len: 14
        encoding: ascii
      - id: specimen_processed_date_time
        type: str
        len: 14
        encoding: ascii
      - id: processing_priority
        type: str
        len: 2
        encoding: ascii
      - id: requesting_facility
        type: str
        len: 20
        encoding: ascii
      - id: requesting_facility_address
        type: str
        len: 50
        encoding: ascii
      - id: requesting_facility_phone_number
        type: str
        len: 20
        encoding: ascii
      - id: requesting_facility_id
        type: str
        len: 20
        encoding: ascii
      - id: specimen_barcode
        type: str
        len: 20
        encoding: ascii
      - id: parent_result
        type: str
        len: 20
        encoding: ascii
      - id: observation_result_sequence_number
        type: str
        len: 10
        encoding: ascii
      - id: parent_observation_sequence_number
        type: str
        len: 10
        encoding: ascii
    doc: Observation Request Segment

  obx_segment:
    seq:
      - id: set_id
        type: str
        len: 4
        encoding: ascii
      - id: value_type
        type: str
        len: 2
        encoding: ascii
      - id: observation_identifier
        type: str
        len: 20
        encoding: ascii
      - id: observation_sub_identifier
        type: str
        len: 20
        encoding: ascii
      - id: observation_value
        type: str
        len: 50
        encoding: ascii
      - id: units
        type: str
        len: 10
        encoding: ascii
      - id: reference_range
        type: str
        len: 20
        encoding: ascii
      - id: abnormal_flags
        type: str
        len: 10
        encoding: ascii
      - id: probability
        type: str
        len: 10
        encoding: ascii
      - id: nature_of_abnormal_test
        type: str
        len: 10
        encoding: ascii
      - id: observation_result_status
        type: str
        len: 1
        encoding: ascii
      - id: effective_date_of_reference_range
        type: str
        len: 8
        encoding: ascii
      - id: user_defined_access_checks
        type: str
        len: 10
        encoding: ascii
      - id: observation_date_time
        type: str
        len: 14
        encoding: ascii
      - id: producer_id
        type: str
        len: 20
        encoding: ascii
      - id: responsible_observer
        type: str
        len: 20
        encoding: ascii
      - id: observation_method
        type: str
        len: 10
        encoding: ascii
      - id: equipment_instance_identifier
        type: str
        len: 20
        encoding: ascii
      - id: observation_identifier_list
        type: str
        len: 20
        encoding: ascii
    doc: Observation Result Segment