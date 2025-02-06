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
      - id: segment_content
        type:
          switch-on: segment_type
          cases:
            '"MSH"': msh_segment
            '"PID"': pid_segment
            '"PV1"': pv1_segment
            '"OBR"': obr_segment
            '"OBX"': obx_segment
            _: unknown_segment

  field:
    seq:
      - id: content
        type: str
        terminator: 0x7C # |
        encoding: ASCII

  component:
    seq:
      - id: parts
        type: str
        terminator: 0x5E # ^
        encoding: ASCII

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
        type: field
      - id: sending_facility
        type: field
      - id: receiving_application
        type: field
      - id: receiving_facility
        type: field
      - id: datetime
        type: field
      - id: security
        type: field
      - id: message_type
        type: field
      - id: message_control_id
        type: field
      - id: processing_id
        type: field
      - id: version_id
        type: field
      - id: sequence_number
        type: field
      - id: continuation_pointer
        type: field
      - id: accept_ack_type
        type: field
      - id: application_ack_type
        type: field
      - id: country_code
        type: field
      - id: character_set
        type: field
      - id: principal_language
        type: field

  pid_segment:
    seq:
      - id: set_id
        type: field
      - id: patient_id
        type: field
      - id: patient_identifier_list
        type: field
      - id: alternate_patient_id
        type: field
      - id: patient_name
        type: field
      - id: mothers_maiden_name
        type: field
      - id: date_of_birth
        type: field
      - id: administrative_sex
        type: field
      - id: patient_alias
        type: field
      - id: race
        type: field
      - id: patient_address
        type: field
      - id: county_code
        type: field
      - id: phone_number_home
        type: field
      - id: phone_number_business
        type: field
      - id: primary_language
        type: field
      - id: marital_status
        type: field
      - id: religion
        type: field
      - id: patient_account_number
        type: field
      - id: ssn_number
        type: field
      - id: drivers_license
        type: field
      - id: mothers_identifier
        type: field
      - id: ethnic_group
        type: field
      - id: birth_place
        type: field
      - id: multiple_birth_indicator
        type: field
      - id: birth_order
        type: field
      - id: citizenship
        type: field
      - id: veterans_military_status
        type: field
      - id: nationality
        type: field
      - id: patient_death_date_time
        type: field
      - id: patient_death_indicator
        type: field

  pv1_segment:
    seq:
      - id: set_id
        type: field
      - id: patient_class
        type: field
      - id: assigned_patient_location
        type: field
      - id: admission_type
        type: field
      - id: preadmit_number
        type: field
      - id: prior_patient_location
        type: field
      - id: attending_doctor
        type: field
      - id: referring_doctor
        type: field
      - id: consulting_doctor
        type: field
      - id: hospital_service
        type: field
      - id: temporary_location
        type: field
      - id: preadmit_test_indicator
        type: field
      - id: readmission_indicator
        type: field
      - id: admit_source
        type: field
      - id: ambulatory_status
        type: field
      - id: vip_indicator
        type: field
      - id: admitting_doctor
        type: field
      - id: patient_type
        type: field
      - id: visit_number
        type: field
      - id: financial_class
        type: field
      - id: charge_price_indicator
        type: field
      - id: courtesy_code
        type: field
      - id: credit_rating
        type: field
      - id: contract_code
        type: field
      - id: contract_effective_date
        type: field
      - id: contract_amount
        type: field
      - id: contract_period
        type: field
      - id: interest_code
        type: field
      - id: transfer_to_bad_debt_code
        type: field
      - id: transfer_to_bad_debt_date
        type: field
      - id: bad_debt_agency_code
        type: field
      - id: bad_debt_transfer_amount
        type: field
      - id: bad_debt_recovery_amount
        type: field
      - id: delete_account_indicator
        type: field
      - id: delete_account_date
        type: field
      - id: discharge_disposition
        type: field
      - id: discharged_to_location
        type: field
      - id: diet_type
        type: field
      - id: servicing_facility
        type: field
      - id: bed_status
        type: field
      - id: account_status
        type: field
      - id: pending_location
        type: field
      - id: prior_temporary_location
        type: field
      - id: admit_datetime
        type: field
      - id: discharge_datetime
        type: field
      - id: current_patient_balance
        type: field
      - id: total_charges
        type: field
      - id: total_adjustments
        type: field
      - id: total_payments
        type: field
      - id: alternate_visit_id
        type: field
      - id: visit_indicator
        type: field
      - id: other_healthcare_provider
        type: field

  obr_segment:
    seq:
      - id: set_id
        type: field
      - id: placer_order_number
        type: field
      - id: filler_order_number
        type: field
      - id: universal_service_identifier
        type: field
      - id: priority
        type: field
      - id: requested_datetime
        type: field
      - id: observation_datetime
        type: field
      - id: observation_end_datetime
        type: field
      - id: collection_volume
        type: field
      - id: collector_identifier
        type: field
      - id: specimen_action_code
        type: field
      - id: danger_code
        type: field
      - id: relevant_clinical_info
        type: field
      - id: specimen_received_datetime
        type: field
      - id: specimen_source
        type: field
      - id: ordering_provider
        type: field
      - id: order_callback_phone_number
        type: field
      - id: placers_field_1
        type: field
      - id: placers_field_2
        type: field
      - id: filler_field_1
        type: field
      - id: filler_field_2
        type: field
      - id: results_rpt_status_chng_datetime
        type: field
      - id: charge_to_practice
        type: field
      - id: diagnostic_serv_sect_id
        type: field
      - id: result_status
        type: field
      - id: parent_result
        type: field
      - id: quantity_timing
        type: field
      - id: result_copies_to
        type: field
      - id: parent
        type: field
      - id: transportation_mode
        type: field
      - id: reason_for_study
        type: field
      - id: principal_result_interpreter
        type: field
      - id: assistant_result_interpreter
        type: field
      - id: technician
        type: field
      - id: transcriptionist
        type: field
      - id: scheduled_datetime
        type: field
      - id: number_of_sample_containers
        type: field
      - id: transport_logistics_of_sample
        type: field
      - id: collectors_comment
        type: field
      - id: transport_arrangement_responsibility
        type: field
      - id: transport_arranged
        type: field
      - id: escort_required
        type: field
      - id: planned_patient_transport_comment
        type: field
      - id: procedure_code
        type: field
      - id: procedure_code_modifier
        type: field
      - id: placer_supplemental_service_info
        type: field
      - id: filler_supplemental_service_info
        type: field
      - id: medically_necessary_duplicate_proc_reason
        type: field
      - id: result_handling
        type: field

  obx_segment:
    seq:
      - id: set_id
        type: field
      - id: value_type
        type: field
      - id: observation_identifier
        type: field
      - id: observation_sub_id
        type: field
      - id: observation_value
        type: field
      - id: units
        type: field
      - id: references_range
        type: field
      - id: abnormal_flags
        type: field
      - id: probability
        type: field
      - id: nature_of_abnormal_test
        type: field
      - id: observation_result_status
        type: field
      - id: effective_date_of_reference_range
        type: field
      - id: user_defined_access_checks
        type: field
      - id: datetime_of_the_observation
        type: field
      - id: producers_id
        type: field
      - id: responsible_observer
        type: field
      - id: observation_method
        type: field
      - id: equipment_instance_identifier
        type: field
      - id: datetime_of_the_analysis
        type: field

  unknown_segment:
    seq:
      - id: content
        type: str
        size-eos: true
        encoding: ASCII