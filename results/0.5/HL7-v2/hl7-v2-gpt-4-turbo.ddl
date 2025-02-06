grammar HL7v2;

type HL7String = string;
type HL7DateTime = string;
type HL7ID = string;
type HL7Integer = int;

type Component = HL7String;

record Composite {
  components: List[Component] sepBy '^';
}

record MSH {
  fieldSeparator: Char;
  encodingCharacters: HL7String;
  sendingApplication: Composite;
  sendingFacility: Composite;
  receivingApplication: Composite;
  receivingFacility: Composite;
  dateTimeOfMessage: HL7DateTime;
  security: Optional[HL7String];
  messageType: Composite;
  messageControlID: HL7ID;
  processingID: Composite;
  versionID: HL7String;
  sequenceNumber: Optional[HL7String];
  continuationPointer: Optional[HL7String];
  acceptAcknowledgmentType: Optional[Char];
  applicationAcknowledgmentType: Optional[Char];
  countryCode: Optional[HL7String];
  characterSet: Optional[List[HL7String] sepBy '~'];
  principalLanguageOfMessage: Optional[Composite];
  alternateCharacterSetHandlingScheme: Optional[HL7String];
  messageProfileIdentifier: Optional[List[Composite] sepBy '~'];
}

record PID {
  setID: Optional[HL7ID];
  patientID: Optional[List[Composite] sepBy '~'];
  patientIdentifierList: Optional[List[Composite] sepBy '~'];
  alternatePatientID: Optional[List[Composite] sepBy '~'];
  patientName: Optional[List[Composite] sepBy '~'];
  motherMaidenName: Optional[List[Composite] sepBy '~'];
  dateTimeOfBirth: Optional[HL7DateTime];
  administrativeSex: Optional[Char];
  patientAlias: Optional[List[Composite] sepBy '~'];
  race: Optional[List[Composite] sepBy '~'];
  patientAddress: Optional[List[Composite] sepBy '~'];
  countyCode: Optional[HL7String];
  phoneNumberHome: Optional[List[Composite] sepBy '~'];
  phoneNumberBusiness: Optional[List[Composite] sepBy '~'];
  primaryLanguage: Optional[Composite];
  maritalStatus: Optional[Composite];
  religion: Optional[Composite];
  patientAccountNumber: Optional[Composite];
  ssnNumberPatient: Optional[HL7String];
  driverLicenseNumberPatient: Optional[Composite];
  motherIdentifier: Optional[List[Composite] sepBy '~'];
  ethnicGroup: Optional[List[Composite] sepBy '~'];
  birthPlace: Optional[HL7String];
  multipleBirthIndicator: Optional[Char];
  birthOrder: Optional[HL7Integer];
  citizenship: Optional[List[Composite] sepBy '~'];
  veteransMilitaryStatus: Optional[Composite];
  nationality: Optional[Composite];
  patientDeathDateAndTime: Optional[HL7DateTime];
  patientDeathIndicator: Optional[Char];
}

record HL7Message {
  msh: MSH;
  pid: Optional[PID];
}