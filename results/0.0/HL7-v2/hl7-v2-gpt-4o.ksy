meta:
  id: hl7_v2
  title: HL7 Version 2.x Message
  file-extension: hl7
  encoding: ASCII

seq:
  - id: segments
    type: segment
    repeat: eos

types:
  segment:
    seq:
      - id: segment_id
        type: str
        size: 3
      - id: field_separator
        type: str
        size: 1
      - id: fields
        type: field
        repeat: eos

  field:
    seq:
      - id: value
        type: strz
        terminator: '|'
        include: true
        consume: true

enums:
  segment_id:
    msh: "MSH"
    pid: "PID"
    obr: "OBR"
    obx: "OBX"

types:
  msh_segment:
    seq:
      - id: encoding_characters
        type: str
        size: 4
      - id: sending_application
        type: strz
        terminator: '|'
      - id: sending_facility
        type: strz
        terminator: '|'
      - id: receiving_application
        type: strz
        terminator: '|'
      - id: receiving_facility
        type: strz
        terminator: '|'
      - id: date_time_of_message
        type: strz
        terminator: '|'
      - id: security
        type: strz
        terminator: '|'
      - id: message_type
        type: strz
        terminator: '|'
      - id: message_control_id
        type: strz
        terminator: '|'
      - id: processing_id
        type: strz
        terminator: '|'
      - id: version_id
        type: strz
        terminator: '|'

  pid_segment:
    seq:
      - id: set_id
        type: strz
        terminator: '|'
      - id: patient_id
        type: strz
        terminator: '|'
      - id: patient_identifier_list
        type: strz
        terminator: '|'
      - id: alternate_patient_id
        type: strz
        terminator: '|'
      - id: patient_name
        type: strz
        terminator: '|'
      - id: mother's_maiden_name
        type: strz
        terminator: '|'
      - id: date_time_of_birth
        type: strz
        terminator: '|'
      - id: administrative_sex
        type: strz
        terminator: '|'
      - id: patient_alias
        type: strz
        terminator: '|'
      - id: race
        type: strz
        terminator: '|'
      - id: patient_address
        type: strz
        terminator: '|'
      - id: county_code
        type: strz
        terminator: '|'
      - id: phone_number_home
        type: strz
        terminator: '|'
      - id: phone_number_business
        type: strz
        terminator: '|'
      - id: primary_language
        type: strz
        terminator: '|'
      - id: marital_status
        type: strz
        terminator: '|'
      - id: religion
        type: strz
        terminator: '|'
      - id: patient_account_number
        type: strz
        terminator: '|'
      - id: ssn_number_patient
        type: strz
        terminator: '|'
      - id: driver's_license_number_patient
        type: strz
        terminator: '|'
      - id: mother's_identifier
        type: strz
        terminator: '|'
      - id: ethnic_group
        type: strz
        terminator: '|'
      - id: birth_place
        type: strz
        terminator: '|'
      - id: multiple_birth_indicator
        type: strz
        terminator: '|'
      - id: birth_order
        type: strz
        terminator: '|'
      - id: citizenship
        type: strz
        terminator: '|'
      - id: veterans_military_status
        type: strz
        terminator: '|'
      - id: nationality
        type: strz
        terminator: '|'
      - id: patient_death_date_and_time
        type: strz
        terminator: '|'
      - id: patient_death_indicator
        type: strz
        terminator: '|'

  obr_segment:
    seq:
      - id: set_id
        type: strz
        terminator: '|'
      - id: placer_order_number
        type: strz
        terminator: '|'
      - id: filler_order_number
        type: strz
        terminator: '|'
      - id: universal_service_identifier
        type: strz
        terminator: '|'
      - id: priority
        type: strz
        terminator: '|'
      - id: requested_date_time
        type: strz
        terminator: '|'
      - id: observation_date_time
        type: strz
        terminator: '|'
      - id: observation_end_date_time
        type: strz
        terminator: '|'
      - id: collection_volume
        type: strz
        terminator: '|'
      - id: collector_identifier
        type: strz
        terminator: '|'
      - id: specimen_action_code
        type: strz
        terminator: '|'
      - id: danger_code
        type: strz
        terminator: '|'
      - id: relevant_clinical_information
        type: strz
        terminator: '|'
      - id: specimen_received_date_time
        type: strz
        terminator: '|'
      - id: specimen_source
        type: strz
        terminator: '|'
      - id: ordering_provider
        type: strz
        terminator: '|'
      - id: order_callback_phone_number
        type: strz
        terminator: '|'
      - id: placer_field_1
        type: strz
        terminator: '|'
      - id: placer_field_2
        type: strz
        terminator: '|'
      - id: filler_field_1
        type: strz
        terminator: '|'
      - id: filler_field_2
        type: strz
        terminator: '|'
      - id: results_rpt_status_chng_date_time
        type: strz
        terminator: '|'
      - id: charge_to_practice
        type: strz
        terminator: '|'
      - id: diagnostic_serv_sect_id
        type: strz
        terminator: '|'
      - id: result_status
        type: strz
        terminator: '|'
      - id: parent_result
        type: strz
        terminator: '|'
      - id: quantity_timing
        type: strz
        terminator: '|'
      - id: result_copies_to
        type: strz
        terminator: '|'
      - id: parent
        type: strz
        terminator: '|'
      - id: transportation_mode
        type: strz
        terminator: '|'
      - id: reason_for_study
        type: strz
        terminator: '|'
      - id: principal_result_interpreter
        type: strz
        terminator: '|'
      - id: assistant_result_interpreter
        type: strz
        terminator: '|'
      - id: technician
        type: strz
        terminator: '|'
      - id: transcriptionist
        type: strz
        terminator: '|'
      - id: scheduled_date_time
        type: strz
        terminator: '|'
      - id: number_of_sample_containers
        type: strz
        terminator: '|'
      - id: transport_logistics_of_collected_sample
        type: strz
        terminator: '|'
      - id: collector_comment
        type: strz
        terminator: '|'
      - id: transport_arrangement_responsibility
        type: strz
        terminator: '|'
      - id: transport_arranged
        type: strz
        terminator: '|'
      - id: escort_required
        type: strz
        terminator: '|'
      - id: planned_patient_transport_comment
        type: strz
        terminator: '|'

  obx_segment:
    seq:
      - id: set_id
        type: strz
        terminator: '|'
      - id: value_type
        type: strz
        terminator: '|'
      - id: observation_identifier
        type: strz
        terminator: '|'
      - id: observation_sub_id
        type: strz
        terminator: '|'
      - id: observation_value
        type: strz
        terminator: '|'
      - id: units
        type: strz
        terminator: '|'
      - id: references_range
        type: strz
        terminator: '|'
      - id: abnormal_flags
        type: strz
        terminator: '|'
      - id: probability
        type: strz
        terminator: '|'
      - id: nature_of_abnormal_test
        type: strz
        terminator: '|'
      - id: observation_result_status
        type: strz
        terminator: '|'
      - id: effective_date_of_reference_range
        type: strz
        terminator: '|'
      - id: user_defined_access_checks
        type: strz
        terminator: '|'
      - id: date_time_of_the_observation
        type: strz
        terminator: '|'
      - id: producer_id
        type: strz
        terminator: '|'
      - id: responsible_observer
        type: strz
        terminator: '|'
      - id: observation_method
        type: strz
        terminator: '|'