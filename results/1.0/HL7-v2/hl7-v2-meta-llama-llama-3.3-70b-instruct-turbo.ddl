format hl7 {
  segment MSH {
    field separator: char,
    encoding_characters: char,
    sending_facility: string(20),
    sending_application: string(20),
    receiving_facility: string(20),
    receiving_application: string(20),
    date: date(),
    time: time(),
    security: enum { LOW, MEDIUM, HIGH },
    message_type: string(3),
    message_control_id: string(20),
    processing_id: enum { P, T, D },
    version_id: string(20),
    sequence_number: int,
    continuation_pointer: string(180),
    accept_acknowledgment_type: enum { AL, NE, ER },
    application_acknowledgment_type: enum { AL, NE, ER },
    country_code: string(20),
    character_set: string(20),
    principal_language_of_message: string(20),
  }

  segment EVN {
    event_type_code: string(3),
    recorded_date_time: datetime(),
    date_time_planned: datetime(),
    event_reason_code: string(4),
    operator_id: string(20),
    event_occurrence_span: string(2),
    event_facility: string(20),
  }

  segment PID {
    set_id: int,
    patient_id: string(20),
    patient_id_external: string(20),
    patient_name: string(50),
    mothers_maiden_name: string(50),
    date_of_birth: date(),
    sex: enum { M, F, U, A, N, O },
    patient_alias: string(50),
    race: string(20),
    patient_address: string(50),
    county_code: string(20),
    phone_number_home: string(20),
    phone_number_business: string(20),
    patient_account_number: string(20),
    social_security_number: string(20),
    drivers_license_number: string(20),
    mothers_id: string(20),
  }

  segment PV1 {
    set_id: int,
    patient_visit_number: string(20),
    visit_number: string(20),
    patient_class: enum { I, O, E, C },
    visit_number_internal: string(20),
    admission_type: enum { E, U, C, N },
    admit_date_time: datetime(),
    discharge_disposition: enum { 01, 02, 03, 04 },
    discharge_date_time: datetime(),
    visit_number_external: string(20),
    patient_account_number: string(20),
    admitting_doctor_id: string(20),
    visit_description: string(20),
  }

  segment ORC {
    order_control: enum { NW, OK, CA, OC },
    order_number: string(20),
    order_type: string(3),
    order_status: enum { NW, CA, OC, IP },
    response_flag: enum { Y, N },
    quantity_ordered: int,
    parent_order_number: string(20),
    date_time_of_order: datetime(),
    entered_by: string(20),
    verified_by: string(20),
    order_effective_date_time: datetime(),
    order_control_code_reason: string(3),
    entering_organization: string(20),
    entering_device: string(20),
  }

  segment OBR {
    set_id: int,
    placment_number: string(20),
    filler_order_number: string(20),
    universal_service_id: string(20),
    priority: enum { S, A, R, P },
    requested_date_time: datetime(),
    observation_date_time: datetime(),
    observation_result_status: enum { P, C, F },
    who_subject_definition: string(20),
    observation_result: string(50),
    charge_to_pricing_indicator: enum { Y, N },
    result_status: enum { P, C, F },
    parent_result: string(20),
    transportation_mode: enum { L, A, T },
    reason_for_study: string(50),
    principal_result_interpreter: string(50),
  }

  segment OBX {
    set_id: int,
    value_type: string(3),
    observation_identifier: string(20),
    observation_sub_id: string(20),
    observation_value: string(50),
    units: string(20),
    references_range: string(50),
    abnormal_flags: enum { H, L, N, B, A },
    probability: int,
    nature_of_abnormal_test: enum { B, A },
    observation_result_status: enum { P, C, F },
    effective_date_of_reference_ranges: date(),
    user_defined_access_checks: string(20),
    date_last_observation_normal: date(),
    user_defined_flags: string(20),
  }

  message HL7v2 {
    segment MSH,
    segment EVN,
    segment PID,
    segment PV1,
    segment ORC,
    segment OBR,
    segment OBX,
  }
}