module HL7v2 {
  type MSH = struct {
    fieldSeparator: u8;
    encodingCharacters: string(4);
    sendingApplication: string;
    sendingFacility: string;
    receivingApplication: string;
    receivingFacility: string;
    dateTimeOfMessage: string;
    security: option<string>;
    messageType: string;
    messageControlID: string;
    processingID: string;
    versionID: string;
    sequenceNumber: option<string>;
    continuationPointer: option<string>;
    acceptAcknowledgmentType: option<string>;
    applicationAcknowledgmentType: option<string>;
    countryCode: option<string>;
    characterSet: option<string>;
    principalLanguageOfMessage: option<string>;
  }
}