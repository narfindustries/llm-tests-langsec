struct HL7v2 {
    field_separator: string = "|";
    component_separator: string = "^";
    subcomponent_separator: string = "&";
    repetition_separator: string = "~";
    escape_character: string = "\\";

    message: Message;
}

struct Message {
    header: MSH;
    segments: Segment[];
}

union Segment {
    MSH;
    PID;
    PV1;
    OBR;
    OBX;
    EVN;
    NK1;
    IN1;
    DG1;
}

struct MSH {
    identifier: string = "MSH";
    encoding_chars: string[4];
    sending_app: HD;
    sending_facility: HD;
    receiving_app: HD;
    receiving_facility: HD;
    datetime: DateTime;
    message_type: MessageType;
    message_control_id: string;
    processing_id: string;
    version_id: string;
}

struct HD {
    namespace_id: string;
    universal_id: string;
    universal_id_type: string;
}

struct DateTime {
    date: u8[8];
    time: u8[4..8];
}

struct MessageType {
    message_code: string;
    trigger_event: string;
    message_structure: string;
}

struct PID {
    identifier: string = "PID";
    set_id: u32;
    patient_id: string;
    patient_id_list: PatientId[];
    patient_name: PersonName[];
    maiden_name: string;
    birth_date: u8[8];
    sex: string;
    patient_alias: string[];
    race: string;
    address: Address[];
    phone_home: PhoneNumber[];
    phone_business: PhoneNumber[];
}

struct PatientId {
    id: string;
    check_digit: string;
    code: string;
    assigning_authority: string;
    id_type: string;
}

struct PersonName {
    family_name: string;
    given_name: string;
    middle_name: string;
    suffix: string;
    prefix: string;
}

struct Address {
    street: string;
    city: string;
    state: string;
    zip: string;
    country: string;
    type: string;
    other: string;
    county: string;
}

struct PhoneNumber {
    number: string;
    type: string;
    code: string;
}

struct PV1 {
    identifier: string = "PV1";
    set_id: u32;
    patient_class: string;
    assigned_location: Location[];
    admission_type: string;
    preadmit_number: string;
    attending_doctor: Doctor[];
    admitting_doctor: Doctor[];
    referring_doctor: Doctor[];
    consulting_doctor: Doctor[];
    hospital_service: string;
}

struct Location {
    point_of_care: string;
    room: string;
    bed: string;
    facility: string;
}

struct Doctor {
    id: string;
    family_name: string;
    given_name: string;
}

struct OBR {
    identifier: string = "OBR";
    set_id: u32;
    placer_order_number: string;
    filler_order_number: string;
    universal_service_id: ServiceId[];
    priority: string;
    observation_datetime: DateTime;
    specimen_datetime: DateTime;
    result_status: string;
}

struct ServiceId {
    identifier: string;
    text: string;
    name: string;
    alternate_identifier: string;
    alternate_text: string;
    alternate_name: string;
}

struct OBX {
    identifier: string = "OBX";
    set_id: u32;
    value_type: string;
    observation_id: ObservationId[];
    observation_value: string[];
    units: Unit[];
    references_range: string;
    abnormal_flags: string;
    probability: u32;
    nature_of_abnormal_test: string;
}

struct ObservationId {
    identifier: string;
    text: string;
    name: string;
    alternate_identifier: string;
    alternate_text: string;
    alternate_name: string;
}

struct Unit {
    identifier: string;
    text: string;
    name: string;
}

struct EVN {
    identifier: string = "EVN";
    event_type: string;
    recorded_datetime: DateTime;
    planned_datetime: DateTime;
    event_reason_code: string;
}

struct NK1 {
    identifier: string = "NK1";
    set_id: u32;
    name: string;
    relationship: Relationship[];
    address: string;
    phone_number: string;
}

struct Relationship {
    identifier: string;
    text: string;
    alternate_identifier: string;
}

struct IN1 {
    identifier: string = "IN1";
    set_id: u32;
    insurance_plan_id: InsurancePlan[];
    insurance_company_id: string;
    insurance_company_name: string;
    insurance_company_address: Address[];
    group_number: string;
    group_name: string;
}

struct InsurancePlan {
    identifier: string;
    text: string;
    alternate_identifier: string;
}

struct DG1 {
    identifier: string = "DG1";
    set_id: u32;
    coding_method: string;
    diagnosis_code: DiagnosisCode[];
    description: string;
    datetime: DateTime;
    type: string;
}

struct DiagnosisCode {
    identifier: string;
    text: string;
    alternate_identifier: string;
}