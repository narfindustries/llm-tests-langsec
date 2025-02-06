schema HL7v2;

type HL7v2SegmentHeader = struct {
  segmentType: string(3)
}

type MSH_9 = struct {
  msgType: string,
  triggerEvent: string
}

type MSH = struct {
  header: HL7v2SegmentHeader,
  fieldSeparator: char,
  encodingChars: string(4),
  sendingApplication: string,
  sendingFacility: string,
  receivingApplication: string,
  receivingFacility: string,
  dateTimeOfMessage: string,
  messageType: MSH_9,
  messageControlID: string,
  processingID: string,
  versionID: string
}

type PID = struct {
  header: HL7v2SegmentHeader,
  setID: string,
  patientID: string,
  patientIdentifierList: string,
  alternatePatientID: string,
  patientName: string,
  mothersMaidenName: string,
  dateTimeOfBirth: string,
  sex: char,
  patientAlias: string,
  race: string,
  patientAddress: string,
  countryCode: string(3),
  phoneNumberHome: string,
  phoneNumberBusiness: string,
  primaryLanguage: string,
  maritalStatus: string,
  religion: string,
  patientAccountNumber: string
}

type HL7Message = struct {
  msh: MSH,
  pid: PID
}

protocol HL7 {
  stream of HL7Message
}