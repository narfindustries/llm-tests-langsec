seq:
  - id: msh
    type: msh
  - id: evn
    type: evn
  - id: pid
    type: pid
  - id: pv1
    type: pv1
  - id: orc
    type: orc
  - id: obr
    type: obr
  - id: obx
    type: obx
  - id: al1
    type: al1
  - id: dg1
    type: dg1
  - id: gt1
    type: gt1

types:
  msh:
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
        encoding: ascii
      - id: sending_facility
        type: str
        encoding: ascii
      - id: receiving_application
        type: str
        encoding: ascii
      - id: receiving_facility
        type: str
        encoding: ascii
      - id: date_time_of_message
        type: str
        size: 14
        encoding: ascii
      - id: security
        type: str
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

  evn:
    seq:
      - id: event_type_code
        type: str
        size: 3
        encoding: ascii
      - id: recorded_date_time
        type: str
        size: 14
        encoding: ascii
      - id: date_time_planned_event
        type: str
        size: 14
        encoding: ascii
      - id: event_reason_code
        type: str
        size: 3
        encoding: ascii
      - id: operator_id
        type: str
        encoding: ascii

  pid:
    seq:
      - id: patient_id
        type: str
        encoding: ascii
      - id: patient_id_external
        type: str
        encoding: ascii
      - id: patient_name
        type: str
        encoding: ascii
      - id: mothers_maiden_name
        type: str
        encoding: ascii
      - id: date_of_birth
        type: str
        size: 8
        encoding: ascii
      - id: sex
        type: str
        size: 1
        encoding: ascii
      - id: patient_address
        type: str
        encoding: ascii
      - id: county_code
        type: str
        encoding: ascii
      - id: phone_number_home
        type: str
        encoding: ascii
      - id: phone_number_business
        type: str
        encoding: ascii

  pv1:
    seq:
      - id: visit_number
        type: str
        encoding: ascii
      - id: patient_class
        type: str
        size: 1
        encoding: ascii
      - id: visit_number_external
        type: str
        encoding: ascii
      - id: admission_type
        type: str
        size: 1
        encoding: ascii
      - id: payer_code
        type: str
        encoding: ascii
      - id: visit_date
        type: str
        size: 8
        encoding: ascii
      - id: visit_time
        type: str
        size: 6
        encoding: ascii

  orc:
    seq:
      - id: order_control
        type: str
        size: 2
        encoding: ascii
      - id: placer_order_number
        type: str
        encoding: ascii
      - id: filler_order_number
        type: str
        encoding: ascii
      - id: order_status
        type: str
        size: 2
        encoding: ascii
      - id: response_flag
        type: str
        size: 1
        encoding: ascii
      - id: quantity_timing
        type: str
        encoding: ascii

  obr:
    seq:
      - id: set_id_obr
        type: str
        size: 4
        encoding: ascii
      - id: placer_order_number
        type: str
        encoding: ascii
      - id: filler_order_number
        type: str
        encoding: ascii
      - id: universal_service_identifier
        type: str
        encoding: ascii
      - id: priority
        type: str
        size: 1
        encoding: ascii
      - id: requested_date_time
        type: str
        size: 14
        encoding: ascii

  obx:
    seq:
      - id: set_id_obx
        type: str
        size: 4
        encoding: ascii
      - id: value_type
        type: str
        size: 2
        encoding: ascii
      - id: observation_result
        type: str
        encoding: ascii
        terminator: "\r"
      - id: units
        type: str
        encoding: ascii
        terminator: "\r"
      - id: reference_range
        type: str
        encoding: ascii
        terminator: "\r"
      - id: abnormal_flags
        type: str
        size: 2
        encoding: ascii

  al1:
    seq:
      - id: set_id_al1
        type: str
        size: 4
        encoding: ascii
      - id: allergy_type
        type: str
        encoding: ascii
      - id: allergy_code
        type: str
        encoding: ascii
      - id: allergy_severity
        type: str
        encoding: ascii
      - id: allergy_reaction
        type: str
        encoding: ascii

  dg1:
    seq:
      - id: set_id_dg1
        type: str
        size: 4
        encoding: ascii
      - id: diagnosis_code
        type: str
        encoding: ascii
      - id: diagnosis_code_identifier
        type: str
        encoding: ascii
      - id: diagnosis_type
        type: str
        encoding: ascii
      - id: diagnosis_date
        type: str
        size: 8
        encoding: ascii

  gt1:
    seq:
      - id: set_id_gt1
        type: str
        size: 4
        encoding: ascii
      - id: guarantor_id
        type: str
        encoding: ascii
      - id: guarantor_name
        type: str
        encoding: ascii
      - id: guarantor_address
        type: str
        encoding: ascii
      - id: guarantor_phone_number
        type: str
        encoding: ascii