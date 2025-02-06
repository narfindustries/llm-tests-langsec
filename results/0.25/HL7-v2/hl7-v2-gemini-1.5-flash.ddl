message HL7v2Message {
  MSH: MSHSegment;
  segments: [Segment];
}

message MSHSegment {
  fieldSeparator: String;
  encodingCharacters: String;
  sendingApplication: String;
  sendingFacility: String;
  receivingApplication: String;
  receivingFacility: String;
  dateTime: String;
  security: String;
  messageType: String;
  messageControlID: String;
  processingID: String;
  versionID: String;
  sequenceNumber: Int;
}

message Segment {
  segmentID: String;
  fields: [Field];
}

message Field {
  value: String;
}

main = HL7v2Message {
  MSH: MSHSegment {
    fieldSeparator: "|";
    encodingCharacters: "^~\\&";
    sendingApplication: "MySystem";
    sendingFacility: "MyHospital";
    receivingApplication: "TheirSystem";
    receivingFacility: "TheirHospital";
    dateTime: "20240308100000";
    security: "";
    messageType: "ADT^A01";
    messageControlID: "12345";
    processingID: "P";
    versionID: "2.5";
    sequenceNumber: 1;
  };
  segments: [Segment { segmentID: "PID"; fields: [Field { value: "1234567" }, Field { value: "Doe^John" }] }];
}
