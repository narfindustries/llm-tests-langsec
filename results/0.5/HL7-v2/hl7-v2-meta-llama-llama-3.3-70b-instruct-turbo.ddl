syntax = hl7
type = binary

segment MSH {
  field field_separator: byte = '|';
  field encoding_characters: string = '^~\&';
  field sending_application: string;
  field sending_facility: string;
  field receiving_application: string;
  field receiving_facility: string;
  field date_time_of_message: date_time;
  field message_type: string;
  field message_control_id: string;
  field processing_id: string = 'P' | 'T';
  field version_id: string = '2.5';
  field sequence_number: string;
  field continuation_pointer: string;
  field accept_acknowledgment_type: string = 'AL' | 'NE' | 'ER';
  field accept_application_acknowledgment_type: string = 'AL' | 'NE' | 'ER';
  field country_code: string;
  field character_set: string;
  field principal_language_of_message: string;
}

segment PID {
  field set_id_patient_id: string;
  field patient_id_external_id: string;
  field patient_id_internal_id: string;
  field alternate_patient_id: string;
  field patient_name: string;
  field mother_s_maiden_name: string;
  field date_of_birth: date;
  field sex: string = 'M' | 'F' | 'U';
  field patient_alias: string;
  field race: string;
  field patient_address: string;
  field county_code: string;
  field phone_number_home: string;
  field phone_number_business: string;
  field primary_language: string;
  field marital_status: string;
  field religion: string;
  field patient_account_number: string;
  field ssn_number_patient: string;
  field driver_s_license_number_patient: string;
  field mother_s_name: string;
  field birth_place: string;
  field death_date: date;
  field death_place: string;
  field patient_death_indicator: string = 'Y' | 'N';
  field identity_unknown_indicator: string = 'Y' | 'N';
  field identity_reliability_code: string;
  field last_update_date_time: date_time;
  field last_update_facility: string;
  field species_code: string;
  field breed_code: string;
  field strain: string;
  field production_class_code: string;
}

segment PV1 {
  field set_id_patient_visit: string;
  field visit_number: string;
  field patient_class: string = 'I' | 'O' | 'E';
  field visit_type: string = 'PRE' | 'INP' | 'OUT';
  field admitting_doctor: string;
  field admitting_doctor_id: string;
  field visit_description: string;
  field admit_date_time: date_time;
  field discharge_date_time: date_time;
  field visit_number_other: string;
  field patient_type: string;
  field visit_responsible_organization: string;
  field admitting_doctor_name: string;
  field admitting_doctor_id: string;
  field consult_doctor: string;
  field consult_doctor_id: string;
  field discharge_disposition: string;
  field discharged_to_location: string;
  field diet_type: string;
  field servicing_facility: string;
  field bed_status: string;
  field bed_location: string;
  field admit_source: string;
  field admitting_doctor_name: string;
  field patient_location: string;
  field patient_location_room: string;
  field patient_location_bed: string;
  field patient_location_building: string;
  field patient_location_floor: string;
  field patient_location_address: string;
  field alternate_visit_id: string;
  field patient_status: string;
  field prior_patient_location: string;
  field transfer_to_location: string;
  field current_patient_location: string;
  field awaiting_patient_location: string;
  field pending_location: string;
  field prior_pending_location: string;
  field visit_priority_code: string;
  field predicted_stay: string;
  field visit_number_other: string;
  field patient_visit_status: string;
  field visit_stop_date_time: date_time;
  field visit_start_date_time: date_time;
  field visit_description: string;
}

segment ORC {
  field order_control: string = 'NW' | 'CA' | 'OC' | 'OD' | 'OA' | 'OR' | 'OU' | 'OW' | 'UD';
  field placer_order_number: string;
  field filler_order_number: string;
  field placer_group_number: string;
  field order_status: string = 'IP' | 'CM' | 'CA' | 'DC' | 'SN' | 'SC' | 'ER' | 'HD' | 'CM';
  field response_flag: string = 'Y' | 'N' | 'R';
  field quantity_timing: string;
  field parent_order: string;
  field date_time_of_transaction: date_time;
  field entered_by: string;
  field verified_by: string;
  field ordered_by: string;
  field order_status_modifier: string;
  field advanced_beneficiary_notice_code: string;
  field filling_status: string;
  field prescriber_id: string;
  field phone_number: string;
  field order_type: string;
  field enterer_authorization_mode: string;
  field parent_universal_service_identifier: string;
}

segment OBR {
  field set_id_observation_request: string;
  field placer_order_number: string;
  field filler_order_number: string;
  field universal_service_identifier: string;
  field priority: string = 'S' | 'A' | 'R' | 'P';
  field requested_date_time: date_time;
  field observation_result_status: string = 'P' | 'C' | 'R';
  field who_subject_definition: string;
  field specimen_action_code: string = 'S' | 'R' | 'A' | 'X';
  field danger_code: string;
  field patient_result_status: string = 'P' | 'C' | 'R';
  field quantity_timing: string;
  field result_status_modifier: string;
  field charges: string;
  field diagnostic_serv_sect_id: string;
  field result_location: string;
  field result_orientation: string;
  field specimen_source: string;
  field specimen_description: string;
  field addendum_status: string = 'Final' | 'Preliminary' | 'Corrected';
  field cancelled_as_of_date_time: date_time;
  field specimen_rejected_as_of_date_time: date_time;
  field specimen_received_date_time: date_time;
  field specimen_collection_date_time: date_time;
  field specimen_availability: string = 'Available' | 'Not Available';
  field expected_specimen_volume: string;
  field actual_specimen_volume: string;
  field collection_volume_unit: string;
  field specimen_description: string;
}

segment OBX {
  field set_id_observation_result: string;
  field value_type: string = 'AD' | 'CE' | 'CF' | 'CK' | 'CM' | 'CN' | 'CP' | 'CX' | 'ED' | 'FT' | 'ID' | 'IS' | 'MO' | 'NM' | 'PN' | 'PT' | 'PX' | 'RP' | 'SN' | 'ST' | 'TC' | 'TN' | 'TS' | 'TX' | 'URI' | 'XAD' | 'XPN';
  field observation_identifier: string;
  field observation_sub_id: string;
  field observation_value: string;
  field units: string;
  field references_range: string;
  field abnormal_flags: string = 'H' | 'L' | 'HH' | 'LL';
  field probability: string;
  field nature_of_abnormal_test: string = 'A' | 'N';
  field observation_result_status: string = 'P' | 'C' | 'R';
  field effective_date_of_reference_range: date;
  field user_defined_access_checks: string;
  field date_last_observation_normal_value: date;
  field user_defined: string;
  field document_type: string;
  field maturity_number: string;
  field result_status_modifier: string;
  field parent_result: string;
  field observation_result_set_status: string;
  field effective_date_time_of_reference_range: date_time;
  field collection_volume: string;
  field specimen_rejected_reason: string;
  field detailed_collection_information: string;
}

message HL7 {
  segment MSH;
  segment PID;
  segment PV1;
  segment ORC;
  segment OBR;
  segment OBX;
}