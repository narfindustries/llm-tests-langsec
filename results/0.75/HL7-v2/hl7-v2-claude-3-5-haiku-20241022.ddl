module hl7_v2;

type Optional<T> = union {
    None,
    Some(T)
};

type Delimiters = struct {
    field: char,
    component: char,
    repetition: char,
    escape: char,
    subcomponent: char
};

type DateTime = struct {
    year: uint16,
    month: uint8,
    day: uint8,
    hour: uint8,
    minute: uint8,
    second: Optional<uint8>
};

type Name = struct {
    last: string,
    first: string,
    middle: Optional<string>,
    suffix: Optional<string>
};

type Address = struct {
    street: string,
    city: string,
    state: string,
    postal_code: string,
    country: Optional<string>
};

type CodeValue = struct {
    code: string,
    description: Optional<string>,
    system: Optional<string>
};

type MessageHeader = struct {
    delimiters: Delimiters,
    sending_app: Optional<string>,
    sending_facility: Optional<string>,
    receiving_app: Optional<string>,
    receiving_facility: Optional<string>,
    timestamp: DateTime,
    message_type: CodeValue,
    version: string,
    processing_id: CodeValue
};

type PatientIdentification = struct {
    ids: [*]string,
    names: [*]Name,
    birth_date: Optional<DateTime>,
    gender: Optional<char>,
    addresses: [*]Address,
    phone_numbers: [*]string,
    language: Optional<CodeValue>,
    marital_status: Optional<CodeValue>
};

type Location = struct {
    facility: Optional<string>,
    ward: Optional<string>,
    room: Optional<string>,
    bed: Optional<string>
};

type Provider = struct {
    id: string,
    name: Name,
    role: Optional<CodeValue>
};

type PatientVisit = struct {
    class: Optional<char>,
    location: Optional<Location>,
    attending_physician: Optional<Provider>,
    admission_type: Optional<CodeValue>,
    visit_number: Optional<string>
};

type HL7Message = struct {
    header: MessageHeader,
    patient: PatientIdentification,
    visit: PatientVisit
};