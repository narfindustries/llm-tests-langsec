format HL7v2;

segment MSH {
    FieldSeparator: char;
    EncodingCharacters: string(4);
    SendingApplication: string;
    SendingFacility: string;
    ReceivingApplication: string;
    ReceivingFacility: string;
    DateTimeOfMessage: datetime;
    Security: string optional;
    MessageType: string;
    MessageControlID: string;
    ProcessingID: string;
    VersionID: string;
    SequenceNumber: int optional;
    ContinuationPointer: string optional;
    AcceptAcknowledgmentType: string optional;
    ApplicationAcknowledgmentType: string optional;
    CountryCode: string optional;
}

segment PID {
    SetIDPID: int optional;
    PatientID: string optional;
    PatientIdentifierList: string;
    AlternatePatientIDPID: string optional;
    PatientName: string;
    MothersMaidenName: string optional;
    DateTimeOfBirth: date;
    AdministrativeSex: string;
    PatientAlias: string optional;
    Race: string optional;
    PatientAddress: string optional;
    CountyCode: string optional;
    PhoneNumberHome: string optional;
    PhoneNumberBusiness: string optional;
    PrimaryLanguage: string optional;
    MaritalStatus: string optional;
    Religion: string optional;
    PatientAccountNumber: string optional;
    SSNNumberPatient: string optional;
    DriversLicenseNumberPatient: string optional;
    MothersIdentifier: string optional;
    EthnicGroup: string optional;
    BirthPlace: string optional;
    MultipleBirthIndicator: string optional;
    BirthOrder: int optional;
    Citizenship: string optional;
    VeteransMilitaryStatus: string optional;
    Nationality: string optional;
    PatientDeathDateAndTime: datetime optional;
    PatientDeathIndicator: string optional;
}

segment OBR {
    SetIDOBR: int optional;
    PlacerOrderNumber: string optional;
    FillerOrderNumber: string optional;
    UniversalServiceIdentifier: string;
    Priority: string optional;
    RequestedDateTime: datetime optional;
    ObservationDateTime: datetime;
    ObservationEndDateTime: datetime optional;
    CollectionVolume: string optional;
    CollectorIdentifier: string optional;
    SpecimenActionCode: string optional;
    DangerCode: string optional;
    RelevantClinicalInformation: string optional;
    SpecimenReceivedDateTime: datetime optional;
    SpecimenSource: string optional;
    OrderingProvider: string optional;
    OrderCallbackPhoneNumber: string optional;
    PlacerField1: string optional;
    PlacerField2: string optional;
    FillerField1: string optional;
    FillerField2: string optional;
    ResultsRptStatusChngDateTime: datetime optional;
    ChargeToPractice: string optional;
    DiagnosticServSectID: string optional;
    ResultStatus: string optional;
    ParentResult: string optional;
    QuantityTiming: string optional;
    ResultCopiesTo: string optional;
    Parent: string optional;
    TransportationMode: string optional;
    ReasonForStudy: string optional;
    PrincipalResultInterpreter: string optional;
    AssistantResultInterpreter: string optional;
    Technician: string optional;
    Transcriptionist: string optional;
    ScheduledDateTime: datetime optional;
    NumberOfSampleContainers: int optional;
    TransportLogisticsOfCollectedSample: string optional;
    CollectorsComment: string optional;
    TransportArrangementResponsibility: string optional;
    TransportArranged: string optional;
    EscortRequired: string optional;
    PlannedPatientTransportComment: string optional;
}

segment OBX {
    SetIDOBX: int optional;
    ValueType: string;
    ObservationIdentifier: string;
    ObservationSubID: string optional;
    ObservationValue: string;
    Units: string optional;
    ReferencesRange: string optional;
    AbnormalFlags: string optional;
    Probability: string optional;
    NatureOfAbnormalTest: string optional;
    ObservationResultStatus: string;
    EffectiveDateOfReferenceRange: datetime optional;
    UserDefinedAccessChecks: string optional;
    DateTimeOfTheObservation: datetime optional;
    ProducersID: string optional;
    ResponsibleObserver: string optional;
    ObservationMethod: string optional;
}

segment AL1 {
    SetIDAL1: int optional;
    AllergenTypeCode: string;
    AllergyCodeMnemonicDescription: string;
    AllergySeverityCode: string optional;
    AllergyReactionCode: string optional;
    IdentificationDate: date optional;
}