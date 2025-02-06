module HL7V2

type Delimiters = {
  field: char,
  component: char,
  repetition: char,
  escape: char,
  subcomponent: char
}

type CodedElement = {
  identifier: string,
  text: option string,
  nameOfCodingSystem: option string
}

type PersonName = {
  familyName: string,
  givenName: string,
  middleName: option string,
  suffix: option string,
  prefix: option string
}

type Address = {
  streetAddress: option string,
  otherDesignation: option string,
  city: option string,
  stateOrProvince: option string,
  zipOrPostalCode: option string,
  country: option string
}

type Patient = {
  id: string,
  name: PersonName,
  dateOfBirth: option string,
  administrativeSex: option ('M' | 'F' | 'O' | 'U'),
  race: option CodedElement,
  addresses: list Address
}

type MessageHeader = {
  sendingApplication: string,
  sendingFacility: string,
  receivingApplication: string,
  receivingFacility: string,
  dateTimeOfMessage: string,
  securityCode: option string,
  messageType: string,
  processingId: string,
  versionId: string
}

type PatientVisit = {
  patientClass: 'I' | 'O' | 'E' | 'A',
  assignedPatientLocation: option string,
  admissionType: option string
}

type OrderRequest = {
  placerOrderNumber: option string,
  fillerOrderNumber: option string,
  universalServiceIdentifier: CodedElement,
  requestedDateTime: option string
}

type Observation = {
  valueType: string,
  observationIdentifier: CodedElement,
  observationValue: option string,
  units: option CodedElement,
  referenceRange: option {
    low: float,
    high: float
  }
}

type Segment = 
  | MSH of MessageHeader
  | PID of Patient
  | PV1 of PatientVisit
  | OBR of OrderRequest
  | OBX of Observation

type HL7Message = {
  delimiters: Delimiters,
  segments: list Segment
}

val parseCodedElement: parser CodedElement
val parsePersonName: parser PersonName
val parseAddress: parser Address
val parsePatient: parser Patient
val parseMessageHeader: parser MessageHeader
val parsePatientVisit: parser PatientVisit
val parseOrderRequest: parser OrderRequest
val parseObservation: parser Observation
val parseSegment: parser Segment

val parseDelimiters: parser Delimiters
  = do
    field <- anyChar
    component <- anyChar
    repetition <- anyChar
    escape <- anyChar
    subcomponent <- anyChar
    return { field, component, repetition, escape, subcomponent }

val parseHL7Message: parser HL7Message
  = do
    delimiters <- parseDelimiters
    segments <- many1 parseSegment
    return { delimiters, segments }