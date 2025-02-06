module HL7v2 {
  record MSH {
    string fieldSeparator = "|";
    string encodingCharacters = "^~\\&";
    string sendingApplication;
    string sendingFacility;
    string receivingApplication;
    string receivingFacility;
    string dateTimeOfMessage;
    string security;
    string messageType;
    string messageControlID;
    string processingID;
    string versionID;
    string sequenceNumber;
    string continuationPointer;
    string acceptAcknowledgmentType;
    string applicationAcknowledgmentType;
    string countryCode;
    string characterSet;
    string principalLanguageOfMessage;
  }

  record PID {
    string setIDPID;
    string patientID;
    string patientIdentifierList;
    string alternatePatientIDPID;
    string patientName;
    string mothersMaidenName;
    string dateTimeOfBirth;
    string administrativeSex;
    string patientAlias;
    string race;
    string patientAddress;
    string countyCode;
    string phoneNumberHome;
    string phoneNumberBusiness;
    string primaryLanguage;
    string maritalStatus;
    string religion;
    string patientAccountNumber;
    string ssnNumberPatient;
    string driversLicenseNumberPatient;
    string mothersIdentifier;
    string ethnicGroup;
    string birthPlace;
    string multipleBirthIndicator;
    string birthOrder;
    string citizenship;
    string veteransMilitaryStatus;
    string nationality;
    string patientDeathDateAndTime;
    string patientDeathIndicator;
  }

  record OBR {
    string setIDOBR;
    string placerOrderNumber;
    string fillerOrderNumber;
    string universalServiceID;
    string priority;
    string requestedDateTime;
    string observationDateTime;
    string observationEndDateTime;
    string collectionVolume;
    string collectorIdentifier;
    string specimenActionCode;
    string dangerCode;
    string relevantClinicalInfo;
    string specimenReceivedDateTime;
    string specimenSource;
    string orderingProvider;
    string orderCallbackPhoneNumber;
    string placerField1;
    string placerField2;
    string fillerField1;
    string fillerField2;
    string resultsRptStatusChngDateTime;
    string chargeToPractice;
    string diagnosticServSectID;
    string resultStatus;
    string parentResult;
    string quantityTiming;
    string resultCopiesTo;
    string parent;
    string transportationMode;
    string reasonForStudy;
    string principalResultInterpreter;
    string assistantResultInterpreter;
    string technician;
    string transcriptionist;
    string scheduledDateTime;
    string numberOfSampleContainers;
    string transportLogisticsOfCollectedSample;
    string collectorsComment;
    string transportArrangementResponsibility;
    string transportArranged;
    string escortRequired;
    string plannedPatientTransportComment;
  }

  record OBX {
    string setIDOBX;
    string valueType;
    string observationIdentifier;
    string observationSubID;
    string observationValue;
    string units;
    string referencesRange;
    string abnormalFlags;
    string probability;
    string natureOfAbnormalTest;
    string observationResultStatus;
    string effectiveDateOfReferenceRange;
    string userDefinedAccessChecks;
    string dateTimeOfTheObservation;
    string producersID;
    string responsibleObserver;
    string observationMethod;
    string equipmentInstanceIdentifier;
    string dateTimeOfTheAnalysis;
  }

  record Message {
    MSH msh;
    sequence<PID> pid;
    sequence<OBR> obr;
    sequence<OBX> obx;
  }
}