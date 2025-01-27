module HL7.v2.GPT4Turbo {
  import DAEDALUS::Core;

  type MSHSegment = struct {
    fieldSeparator : Char;
    encodingCharacters : array[Char, 4];
    sendingApplication : String;
    sendingFacility : String;
    receivingApplication : String;
    receivingFacility : String;
    dateTimeOfMessage : String;
    security : String;
    messageType : String;
    messageControlID : String;
    processingID : String;
    versionID : String;
  };

  type EVNSegment = struct {
    eventTypeCode : String;
    recordedDateTime : String;
    dateTimePlannedEvent : String;
    eventReasonCode : String;
    operatorID : String;
    eventOccurred : String;
    eventFacility : String;
  };

  type PIDSegment = struct {
    setID : String;
    patientID : String;
    patientIdentifierList : String;
    alternatePatientID : String;
    patientName : String;
    motherMaidenName : String;
    dateTimeOfBirth : String;
    administrativeSex : String;
    patientAlias : String;
    race : String;
    patientAddress : String;
    countyCode : String;
    phoneNumberHome : String;
    phoneNumberBusiness : String;
    primaryLanguage : String;
    maritalStatus : String;
    religion : String;
    patientAccountNumber : String;
    ssnNumberPatient : String;
    driverLicenseNumberPatient : String;
    motherIdentifier : String;
    ethnicGroup : String;
    birthPlace : String;
    multipleBirthIndicator : String;
    birthOrder : String;
    citizenship : String;
    veteransMilitaryStatus : String;
    nationality : String;
    patientDeathDateAndTime : String;
    patientDeathIndicator : String;
  };

  type HL7Message = struct {
    segments : array[Segment];
  };

  type Segment = union {
    msh : MSHSegment;
    evn : EVNSegment;
    pid : PIDSegment;
  };

  type Message = struct {
    header : MSHSegment;
    body : array[Segment];
  };

  type HL7v2 = struct {
    messages : array[Message];
  };

  let hl7MessageParser = parser {
    let segments = many(parseSegment);
    return HL7Message { segments };
  };

  let parseSegment = parser {
    choice {
      when (peekString(3) == "MSH") => parseMSHSegment;
      when (peekString(3) == "EVN") => parseEVNSegment;
      when (peekString(3) == "PID") => parsePIDSegment;
    }
  };

  let parseMSHSegment = parser {
    fieldSeparator <- parseChar;
    encodingCharacters <- parseArray(parseChar, 4);
    sendingApplication <- parseStringUntil('|');
    sendingFacility <- parseStringUntil('|');
    receivingApplication <- parseStringUntil('|');
    receivingFacility <- parseStringUntil('|');
    dateTimeOfMessage <- parseStringUntil('|');
    security <- parseStringUntil('|');
    messageType <- parseStringUntil('|');
    messageControlID <- parseStringUntil('|');
    processingID <- parseStringUntil('|');
    versionID <- parseStringUntil('|');
    return MSHSegment {
      fieldSeparator,
      encodingCharacters,
      sendingApplication,
      sendingFacility,
      receivingApplication,
      receivingFacility,
      dateTimeOfMessage,
      security,
      messageType,
      messageControlID,
      processingID,
      versionID
    };
  };

  let parseEVNSegment = parser {
    eventTypeCode <- parseStringUntil('|');
    recordedDateTime <- parseStringUntil('|');
    dateTimePlannedEvent <- parseStringUntil('|');
    eventReasonCode <- parseStringUntil('|');
    operatorID <- parseStringUntil('|');
    eventOccurred <- parseStringUntil('|');
    eventFacility <- parseStringUntil('|');
    return EVNSegment {
      eventTypeCode,
      recordedDateTime,
      dateTimePlannedEvent,
      eventReasonCode,
      operatorID,
      eventOccurred,
      eventFacility
    };
  };

  let parsePIDSegment = parser {
    setID <- parseStringUntil('|');
    patientID <- parseStringUntil('|');
    patientIdentifierList <- parseStringUntil('|');
    alternatePatientID <- parseStringUntil('|');
    patientName <- parseStringUntil('|');
    motherMaidenName <- parseStringUntil('|');
    dateTimeOfBirth <- parseStringUntil('|');
    administrativeSex <- parseStringUntil('|');
    patientAlias <- parseStringUntil('|');
    race <- parseStringUntil('|');
    patientAddress <- parseStringUntil('|');
    countyCode <- parseStringUntil('|');
    phoneNumberHome <- parseStringUntil('|');
    phoneNumberBusiness <- parseStringUntil('|');
    primaryLanguage <- parseStringUntil('|');
    maritalStatus <- parseStringUntil('|');
    religion <- parseStringUntil('|');
    patientAccountNumber <- parseStringUntil('|');
    ssnNumberPatient <- parseStringUntil('|');
    driverLicenseNumberPatient <- parseStringUntil('|');
    motherIdentifier <- parseStringUntil('|');
    ethnicGroup <- parseStringUntil('|');
    birthPlace <- parseStringUntil('|');
    multipleBirthIndicator <- parseStringUntil('|');
    birthOrder <- parseStringUntil('|');
    citizenship <- parseStringUntil('|');
    veteransMilitaryStatus <- parseStringUntil('|');
    nationality <- parseStringUntil('|');
    patientDeathDateAndTime <- parseStringUntil('|');
    patientDeathIndicator <- parseStringUntil('|');
    return PIDSegment {
      setID,
      patientID,
      patientIdentifierList,
      alternatePatientID,
      patientName,
      motherMaidenName,
      dateTimeOfBirth,
      administrativeSex,
      patientAlias,
      race,
      patientAddress,
      countyCode,
      phoneNumberHome,
      phoneNumberBusiness,
      primaryLanguage,
      maritalStatus,
      religion,
      patientAccountNumber,
      ssnNumberPatient,
      driverLicenseNumberPatient,
      motherIdentifier,
      ethnicGroup,
      birthPlace,
      multipleBirthIndicator,
      birthOrder,
      citizenship,
      veteransMilitaryStatus,
      nationality,
      patientDeathDateAndTime,
      patientDeathIndicator
    };
  };
}