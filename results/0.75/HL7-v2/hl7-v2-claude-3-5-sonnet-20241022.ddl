grammar HL7v2;

@Message = 
  HL7Message;

HL7Message = {
  msh: MSH
  segments*: Segment
};

MSH = {
  "MSH"
  field_sep: "^|"
  encoding_chars: "^~\\&"
  sending_app: FieldContent
  sending_facility: FieldContent
  receiving_app: FieldContent
  receiving_facility: FieldContent
  datetime: DateTime
  security: FieldContent
  message_type: MessageType
  message_control_id: FieldContent
  processing_id: ProcessingID
  version_id: "2.5"
};

Segment =
  PID |
  PV1 |
  OBR |
  OBX;

PID = {
  "PID"
  set_id: FieldContent
  patient_id: FieldContent
  patient_id_list: FieldContent
  alt_patient_id: FieldContent
  patient_name: FieldContent
  mothers_maiden_name: FieldContent
  birth_date: DateTime
  admin_sex: Sex
};

PV1 = {
  "PV1"
  set_id: FieldContent
  patient_class: FieldContent
  assigned_location: FieldContent
  admission_type: FieldContent
  preadmit_number: FieldContent
  prior_location: FieldContent
};

OBR = {
  "OBR"
  set_id: FieldContent
  placer_order_number: FieldContent
  filler_order_number: FieldContent
  universal_service_id: FieldContent
  priority: FieldContent
  requested_datetime: DateTime
};

OBX = {
  "OBX"
  set_id: FieldContent
  value_type: FieldContent
  observation_id: FieldContent
  observation_value: FieldContent
  units: FieldContent
  reference_range: FieldContent
  abnormal_flags: FieldContent
};

FieldContent = {
  content: /[^|~\\\r\n]*/
};

DateTime = {
  date: /\d{8}/
  time: /\d{4}/
};

MessageType = {
  message_code: /[A-Z]{3}/
  trigger_event: /[A-Z0-9]{3}/
  message_structure: /[A-Z]{3}/
};

ProcessingID = {
  id: /[PTA]/
  mode: /[NID]/
};

Sex = {
  value: /[MFO]/
};