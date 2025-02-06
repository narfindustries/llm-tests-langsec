HL7v2 = {
  MSH: {
    FieldSeparator: '|',
    EncodingCharacters: '^~\&',
    SendingApplication: string,
    SendingFacility: string,
    ReceivingApplication: string,
    ReceivingFacility: string,
    DateTimeOfMessage: string,
    Security: optional(string),
    MessageType: {
      MessageCode: string,
      TriggerEvent: string
    },
    MessageControlID: string,
    ProcessingID: {
      ProcessingMode: ['P', 'T', 'D']
    },
    VersionID: string
  },
  PID: {
    SetID: optional(string),
    PatientID: optional(string),
    PatientIdentifierList: optional([string]),
    PatientName: optional(string),
    DateOfBirth: optional(string),
    Sex: optional(['M', 'F', 'U']),
    PatientAddress: optional(string),
    PhoneNumber: optional(string)
  },
  PV1: {
    SetID: optional(string),
    PatientClass: optional(['I', 'O', 'E']),
    AssignedPatientLocation: optional(string),
    AttendingDoctor: optional(string),
    ReferringDoctor: optional(string),
    ConsultingDoctor: optional(string)
  },
  OBX: {
    SetID: optional(string),
    ValueType: optional(['ST', 'NM', 'DT', 'TM', 'TS', 'FT', 'TX', 'SN']),
    ObservationIdentifier: optional(string),
    ObservationValue: optional(string),
    Units: optional(string),
    ObservationResultStatus: optional(['F', 'P', 'C', 'X'])
  }
}