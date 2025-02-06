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
        size: 14
        encoding: ascii
      - id: security
        type: str
        size: 40
        encoding: ascii
      - id: message_type
        type: str
        size: 15
        encoding: ascii
      - id: message_control_id
        type: str
        size: 20
        encoding: ascii
      - id: processing_id
        type: str
        size: 2
        encoding: ascii
      - id: version_id
        type: str
        size: 10
        encoding: ascii
      - id: sequence_number
        type: str
        size: 15
        encoding: ascii
        if: sequence_number_present
      - id: continuation_pointer
        type: str
        size: 180
        encoding: ascii
        if: continuation_pointer_present
      - id: accept_acknowledgment_type
        type: str
        size: 2
        encoding: ascii
        if: accept_acknowledgment_type_present
      - id: application_acknowledgment_type
        type: str
        size: 2
        encoding: ascii
        if: application_acknowledgment_type_present
      - id: country_code
        type: str
        size: 10
        encoding: ascii
        if: country_code_present
      - id: character_set
        type: str
        size: 10
        encoding: ascii
        if: character_set_present
      - id: principal_language_of_message
        type: str
        size: 10
        encoding: ascii
        if: principal_language_of_message_present

  pid_segment:
    seq:
      - id: set_id_patient_id
        type: str
        size: 4
        encoding: ascii
      - id: patient_id
        type: str
        size: 20
        encoding: ascii
      - id: patient_id_identifier
        type: str
        size: 20
        encoding: ascii
      - id: alternate_patient_id
        type: str
        size: 20
        encoding: ascii
      - id: patient_name
        type: str
        size: 50
        encoding: ascii
      - id: mother_s_maiden_name
        type: str
        size: 20
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
        size: 20
        encoding: ascii
      - id: race
        type: str
        size: 20
        encoding: ascii
      - id: patient_address
        type: str
        size: 50
        encoding: ascii
      - id: county_code
        type: str
        size: 10
        encoding: ascii
        if: county_code_present
      - id: phone_number_home
        type: str
        size: 20
        encoding: ascii
        if: phone_number_home_present
      - id: phone_number_business
        type: str
        size: 20
        encoding: ascii
        if: phone_number_business_present
      - id: primary_language
        type: str
        size: 10
        encoding: ascii
        if: primary_language_present
      - id: marital_status
        type: str
        size: 1
        encoding: ascii
        if: marital_status_present
      - id: religion
        type: str
        size: 20
        encoding: ascii
        if: religion_present
      - id: patient_account_number
        type: str
        size: 20
        encoding: ascii
        if: patient_account_number_present
      - id: ss_number_patient
        type: str
        size: 20
        encoding: ascii
        if: ss_number_patient_present
      - id: driver_s_license_number_patient
        type: str
        size: 20
        encoding: ascii
        if: driver_s_license_number_patient_present
      - id: mother_s_identifier
        type: str
        size: 20
        encoding: ascii
        if: mother_s_identifier_present
      - id: ethnic_group
        type: str
        size: 20
        encoding: ascii
        if: ethnic_group_present
      - id: birth_place
        type: str
        size: 50
        encoding: ascii
        if: birth_place_present
      - id: death_date
        type: str
        size: 8
        encoding: ascii
        if: death_date_present
      - id: death_place
        type: str
        size: 50
        encoding: ascii
        if: death_place_present
      - id: patient_death_indicator
        type: str
        size: 1
        encoding: ascii
        if: patient_death_indicator_present
      - id: identity_unknown_indicator
        type: str
        size: 1
        encoding: ascii
        if: identity_unknown_indicator_present
      - id: identity_reliability
        type: str
        size: 1
        encoding: ascii
        if: identity_reliability_present
      - id: last_update_date_time
        type: str
        size: 14
        encoding: ascii
        if: last_update_date_time_present
      - id: last_update_facility
        type: str
        size: 20
        encoding: ascii
        if: last_update_facility_present
      - id: species_code
        type: str
        size: 10
        encoding: ascii
        if: species_code_present
      - id: breed_code
        type: str
        size: 10
        encoding: ascii
        if: breed_code_present
      - id: production_class_code
        type: str
        size: 10
        encoding: ascii
        if: production_class_code_present
      - id: tribal_citizenship
        type: str
        size: 20
        encoding: ascii
        if: tribal_citizenship_present

  pv1_segment:
    seq:
      - id: set_id_patient_visit
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
      - id: visit_reason
        type: str
        size: 50
        encoding: ascii
      - id: admission_type
        type: str
        size: 1
        encoding: ascii
      - id: visit_mode
        type: str
        size: 1
        encoding: ascii
      - id: discharge_disposition
        type: str
        size: 1
        encoding: ascii
      - id: discharge_disposition_date
        type: str
        size: 8
        encoding: ascii
      - id: discharge_to_location
        type: str
        size: 50
        encoding: ascii
      - id: diet_type
        type: str
        size: 20
        encoding: ascii
      - id: servicing_facility
        type: str
        size: 20
        encoding: ascii
      - id: bed_status
        type: str
        size: 1
        encoding: ascii
      - id: visit_priority
        type: str
        size: 1
        encoding: ascii
      - id: previous_service_date
        type: str
        size: 8
        encoding: ascii
      - id: expected_admit_date
        type: str
        size: 8
        encoding: ascii
      - id: expected_discharge_date
        type: str
        size: 8
        encoding: ascii
      - id: visit_number_other
        type: str
        size: 20
        encoding: ascii
      - id: visit_part_of_visit_number
        type: str
        size: 20
        encoding: ascii
      - id: visit_outlier_type
        type: str
        size: 1
        encoding: ascii
      - id: visit_outlier_days
        type: str
        size: 5
        encoding: ascii
      - id: guarantee_by_participant
        type: str
        size: 20
        encoding: ascii
      - id: contract_effective_date
        type: str
        size: 8
        encoding: ascii
      - id: contract_amount
        type: str
        size: 10
        encoding: ascii
      - id: contract_code
        type: str
        size: 10
        encoding: ascii
      - id: contract_effective_date_2
        type: str
        size: 8
        encoding: ascii
      - id: contract_amount_2
        type: str
        size: 10
        encoding: ascii
      - id: contract_code_2
        type: str
        size: 10
        encoding: ascii
      - id: visit_indicator
        type: str
        size: 1
        encoding: ascii
      - id: other_healthcare_provider
        type: str
        size: 50
        encoding: ascii
      - id: visit_account_number
        type: str
        size: 20
        encoding: ascii
      - id: admitting_doctor
        type: str
        size: 50
        encoding: ascii
      - id: attending_doctor
        type: str
        size: 50
        encoding: ascii
      - id: consulting_doctor
        type: str
        size: 50
        encoding: ascii
      - id: referring_doctor
        type: str
        size: 50
        encoding: ascii
      - id: admission_referring_doctor
        type: str
        size: 50
        encoding: ascii
      - id: primary_care_provider
        type: str
        size: 50
        encoding: ascii
      - id: visit_type
        type: str
        size: 10
        encoding: ascii
      - id: stop_date
        type: str
        size: 8
        encoding: ascii
      - id: prior_patient_location
        type: str
        size: 50
        encoding: ascii
      - id: admit_date_time
        type: str
        size: 14
        encoding: ascii
      - id: discharge_date_time
        type: str
        size: 14
        encoding: ascii
      - id: visit_number_other_2
        type: str
        size: 20
        encoding: ascii
      - id: visit_part_of_visit_number_2
        type: str
        size: 20
        encoding: ascii
      - id: visit_outlier_type_2
        type: str
        size: 1
        encoding: ascii
      - id: visit_outlier_days_2
        type: str
        size: 5
        encoding: ascii
      - id: guarantee_by_participant_2
        type: str
        size: 20
        encoding: ascii
      - id: contract_effective_date_3
        type: str
        size: 8
        encoding: ascii
      - id: contract_amount_3
        type: str
        size: 10
        encoding: ascii
      - id: contract_code_3
        type: str
        size: 10
        encoding: ascii
      - id: contract_effective_date_4
        type: str
        size: 8
        encoding: ascii
      - id: contract_amount_4
        type: str
        size: 10
        encoding: ascii
      - id: contract_code_4
        type: str
        size: 10
        encoding: ascii
      - id: visit_indicator_2
        type: str
        size: 1
        encoding: ascii
      - id: other_healthcare_provider_2
        type: str
        size: 50
        encoding: ascii
      - id: visit_account_number_2
        type: str
        size: 20
        encoding: ascii
      - id: admitting_doctor_2
        type: str
        size: 50
        encoding: ascii
      - id: attending_doctor_2
        type: str
        size: 50
        encoding: ascii
      - id: consulting_doctor_2
        type: str
        size: 50
        encoding: ascii
      - id: referring_doctor_2
        type: str
        size: 50
        encoding: ascii
      - id: admission_referring_doctor_2
        type: str
        size: 50
        encoding: ascii
      - id: primary_care_provider_2
        type: str
        size: 50
        encoding: ascii
      - id: visit_type_2
        type: str
        size: 10
        encoding: ascii
      - id: stop_date_2
        type: str
        size: 8
        encoding: ascii
      - id: prior_patient_location_2
        type: str
        size: 50
        encoding: ascii
      - id: admit_date_time_2
        type: str
        size: 14
        encoding: ascii
      - id: discharge_date_time_2
        type: str
        size: 14
        encoding: ascii

  obr_segment:
    seq:
      - id: set_id_observation_request
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
        size: 20
        encoding: ascii
      - id: priority
        type: str
        size: 1
        encoding: ascii
      - id: requested_date_time
        type: str
        size: 14
        encoding: ascii
      - id: observation_date_time
        type: str
        size: 14
        encoding: ascii
      - id: observation_result_status
        type: str
        size: 1
        encoding: ascii
      - id: who_subject_definition
        type: str
        size: 50
        encoding: ascii
      - id: specimen_source
        type: str
        size: 20
        encoding: ascii
      - id: specimen_responsible_organization
        type: str
        size: 20
        encoding: ascii
      - id: repeating_interval
        type: str
        size: 10
        encoding: ascii
      - id: specimen_collection_date_time
        type: str
        size: 14
        encoding: ascii
      - id: specimen_received_date_time
        type: str
        size: 14
        encoding: ascii
      - id: specimen_expiration_date_time
        type: str
        size: 14
        encoding: ascii
      - id: specimen_source_site
        type: str
        size: 20
        encoding: ascii
      - id: specimen_site_modifier
        type: str
        size: 20
        encoding: ascii
      - id: specimen_collection_method
        type: str
        size: 20
        encoding: ascii
      - id: specimen_type
        type: str
        size: 20
        encoding: ascii
      - id: specimen_additive
        type: str
        size: 20
        encoding: ascii
      - id: specimen_additive_units
        type: str
        size: 10
        encoding: ascii
      - id: specimen_collection_volume
        type: str
        size: 10
        encoding: ascii
      - id: specimen_collection_volume_units
        type: str
        size: 10
        encoding: ascii
      - id: specimen_container_type
        type: str
        size: 20
        encoding: ascii
      - id: specimen_container_condition
        type: str
        size: 20
        encoding: ascii
      - id: specimen_container_id
        type: str
        size: 20
        encoding: ascii
      - id: specimen_parent_result
        type: str
        size: 20
        encoding: ascii
      - id: specimen_parent_result_numeric
        type: str
        size: 10
        encoding: ascii
      - id: specimen_parent_units
        type: str
        size: 10
        encoding: ascii
      - id: specimen_parent_reference_range
        type: str
        size: 20
        encoding: ascii
      - id: specimen_collection_site
        type: str
        size: 50
        encoding: ascii
      - id: specimen_role
        type: str
        size: 20
        encoding: ascii
      - id: specimen_collection_quantity
        type: str
        size: 10
        encoding: ascii
      - id: specimen_collection_quantity_units
        type: str
        size: 10
        encoding: ascii
      - id: specimen_rejection_reason
        type: str
        size: 20
        encoding: ascii
      - id: specimen_dilution_factor
        type: str
        size: 10
        encoding: ascii
      - id: specimen_dilution_factor_units
        type: str
        size: 10
