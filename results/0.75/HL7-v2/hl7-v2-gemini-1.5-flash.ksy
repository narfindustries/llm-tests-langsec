type: struct
endian: be
docs: >
  Simplified representation of a HL7 v2 message. This is NOT a complete
  specification and omits many details for brevity. Refer to the official
  HL7 v2 documentation for a complete and accurate definition.

fields:
  msh:
    type: struct
    docs: "Message Header Segment (MSH)"
    fields:
      field1:
        type: str
        docs: "Field Separator"
      field2:
        type: str
        docs: "Encoding Characters"
      field3:
        type: str
        docs: "Sending Application"
      field4:
        type: str
        docs: "Sending Facility"
      field5:
        type: str
        docs: "Receiving Application"
      field6:
        type: str
        docs: "Receiving Facility"
      field7:
        type: str
        docs: "DateTime"
      field8:
        type: str
        docs: "Security"
      field9:
        type: str
        docs: "Message Type"
      field10:
        type: str
        docs: "Message Control ID"
      field11:
        type: str
        docs: "Processing ID"
      field12:
        type: str
        docs: "Version ID"
      field13:
        type: str
        docs: "Sequence Number"
      field14:
        type: str
        docs: "Continuation Pointer"
      field15:
        type: str
        docs: "Accept Acknowledgement Type"
      field16:
        type: str
        docs: "Application Acknowledgement Type"
      field17:
        type: str
        docs: "Country Code"
      field18:
        type: str
        docs: "Character Set"
      field19:
        type: str
        docs: "Principal Language of Message"
  segments:
    type: seq
    docs: "Sequence of message segments (excluding MSH)"
    items:
      type: str
      docs: "Each segment is represented as a string. Actual segment parsing requires further decomposition based on segment type."

