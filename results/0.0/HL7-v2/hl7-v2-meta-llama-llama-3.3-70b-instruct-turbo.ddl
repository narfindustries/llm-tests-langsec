format hl7_v2 {
  magic = b"HL7";
  body = msh: segment;
  body += pid: segment;
  body += pv1: segment;
  body += obx: segment;
  body += orc: segment;
  body += zpi: segment;
}

segment msh {
  field field_separator: byte = 124;
  field encoding_characters: bytes = b"^~\\&";
  field sending_application: stringz;
  field sending_facility: stringz;
  field receiving_application: stringz;
  field receiving_facility: stringz;
  field date_time_of_message: timestamp;
  field security: stringz;
  field message_type: stringz;
  field message_control_id: stringz;
  field processing_id: byte = 80 | 84;
  field version_id: stringz = "2.5";
}

segment pid {
  field set_id_patient_id: int8;
  field patient_id: stringz;
  field patient_id_identifier: stringz;
  field alternate_patient_id: stringz;
  field patient_name: stringz;
  field mothers_maiden_name: stringz;
  field date_of_birth: date;
  field sex: byte = 77 | 70 | 85;
  field patient_alias: stringz;
  field race: stringz;
  field patient_address: stringz;
  field county_code: stringz;
}

segment pv1 {
  field set_id_patient_visit: int8;
  field visit_number: stringz;
  field patient_class: byte = 73 | 79 | 69;
  field visit_number: stringz;
  field financial_class: stringz;
  field visit_type: stringz;
  field admitting_doctor: stringz;
  field discharge_disposition: stringz;
}

segment obx {
  field set_id_observation_result: int8;
  field observation_result_id: stringz;
  field observation_result_status: byte = 70 | 80 | 67;
  field observation_value: stringz;
  field units: stringz;
  field reference_range: stringz;
  field abnormal_flags: stringz;
}

segment orc {
  field order_control: bytes = b"NW" | b"OK" | b"CA";
  field order_control_id: stringz;
  field order_status: bytes = b"SC" | b"IP" | b"CM";
  field response_flag: byte = 89 | 78;
  field quantity_timing: stringz;
}

segment zpi {
  field set_id_pathology_specimen: int8;
  field specimen_id: stringz;
  field specimen_source: stringz;
  field specimen_type: stringz;
  field specimen_collection_date: date;
  field specimen_collection_time: time;
}