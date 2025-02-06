message MSH {
  separator: string;
  encodingCharacters: string;
  sendingApplication: string;
  sendingFacility: string;
  receivingApplication: string;
  receivingFacility: string;
  dateTime: string;
  security: string;
  messageType: {
    messageType: string;
    triggerEvent: string;
  };
  messageControlID: string;
  processingID: string;
  versionID: string;
  sequenceNumber: int;
  continuationPointer: string;
  acceptAcknowledgementType: string;
  applicationAcknowledgementType: string;
  countryCode: string;
  characterSet: string;
}

message ADT_A01 {
  msh: MSH;
  evn: {
    eventType: string;
    recordedDateTime: string;
    messageDateTime: string;
  };
  pid: {
    patientID: string;
    patientName: string;
  };
}
