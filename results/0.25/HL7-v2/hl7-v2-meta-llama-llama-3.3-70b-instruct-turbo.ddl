domain hl7_v2_meta {
  version = "3.3";
  description = "HL7 v2 meta message";

  type Message = sequence {
    MSH: Segment_MSH,
    *SFG: Segment_SFG,
    *PID: Segment_PID,
    *PV1: Segment_PV1,
    *ORC: Segment_ORC,
    *OBR: Segment_OBR,
    *OBX: Segment_OBX,
  }

  type Segment_MSH = group {
    seg_id: fixed(3) = "MSH",
    encoding_chars: fixed(4),
    field_separator: fixed(1),
    component_separator: fixed(1),
    repetition_separator: fixed(1),
    concurrency_separator: fixed(1),
    continuation_pointer: fixed(1),
    message_date: fixed(8),
    message_time: fixed(6),
    security: fixed(20),
    message_type: fixed(3),
    trigger_event: fixed(3),
    message_control_id: fixed(20),
    processing_id: fixed(3),
    version_id: fixed(12),
    sequence_number: fixed(15),
    continuation_pointer2: fixed(180),
    accept_acknowledgment_type: fixed(2),
    application_acknowledgment_type: fixed(2),
    country_code: fixed(3),
    character_set: fixed(4),
    principal_language_of_message: fixed(3),
  }

  type Segment_SFG = group {
    seg_id: fixed(3) = "SFG",
    sending_facility: fixed(20),
    sending_facility_namespace_id: fixed(20),
    receiving_facility: fixed(20),
    receiving_facility_namespace_id: fixed(20),
    date: fixed(8),
    time: fixed(6),
  }

  type Segment_PID = group {
    seg_id: fixed(3) = "PID",
    set_id: fixed(4),
    patient_id: fixed(20),
    patient_id_internal: fixed(20),
    patient_name: fixed(50),
    patient_name_type_code: fixed(1),
    date_of_birth: fixed(8),
    sex: fixed(1),
    patient_alias: fixed(50),
    racial_category: fixed(1),
    patient_address: fixed(50),
    county_code: fixed(3),
    phone_number_home: fixed(20),
    phone_number_business: fixed(20),
  }

  type Segment_PV1 = group {
    seg_id: fixed(3) = "PV1",
    set_id: fixed(4),
    visit_number: fixed(20),
    patient_class: fixed(1),
    visit_number_internal: fixed(20),
    admit_source: fixed(3),
    admitting_doctor: fixed(50),
    visit_type: fixed(1),
    admit_date: fixed(8),
    admit_time: fixed(6),
    discharge_disposition: fixed(2),
    discharge_date: fixed(8),
    discharge_time: fixed(6),
  }

  type Segment_ORC = group {
    seg_id: fixed(3) = "ORC",
    order_control: fixed(2),
    placer_order_number: fixed(20),
    filler_order_number: fixed(20),
    placer_group_number: fixed(20),
    order_status: fixed(2),
    response_flag: fixed(2),
    quantity/timing: fixed(80),
    parent: fixed(20),
    date: fixed(8),
    order_type: fixed(2),
    enterer: fixed(50),
    order_enterer_location: fixed(20),
  }

  type Segment_OBR = group {
    seg_id: fixed(3) = "OBR",
    set_id: fixed(4),
    placer_order_number: fixed(20),
    filler_order_number: fixed(20),
    universal_service_identifier: fixed(80),
    priority: fixed(2),
    requested_date: fixed(8),
    requested_time: fixed(6),
    observation_date: fixed(8),
    observation_time: fixed(6),
    collection_volume: fixed(10),
    collector_identifier: fixed(50),
    specimen_action_code: fixed(2),
    danger_code: fixed(5),
  }

  type Segment_OBX = group {
    seg_id: fixed(3) = "OBX",
    set_id: fixed(4),
    value_type: fixed(2),
    observation_identifier: fixed(80),
    observation_sub_id: fixed(20),
    observation_value: fixed(80),
    units: fixed(20),
    reference_range: fixed(20),
    abnormal_flags: fixed(2),
    probability: fixed(10),
    nature_of_abnormal_test: fixed(2),
    observation_result_status: fixed(2),
    effective_date_of_reference_range: fixed(8),
    user_defined_access_checks: fixed(20),
  }
}