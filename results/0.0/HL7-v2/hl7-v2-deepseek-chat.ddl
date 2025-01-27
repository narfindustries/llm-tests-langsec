-- Daedalus specification for HL7 v2.x message parsing
-- Improved and fixed based on the error message

import Daedalus

-- Define the HL7 v2.x message structure
def HL7Message =
  block
    header: Header
    segments: Many Segment

def Header =
  block
    fieldSeparator: Char
    encodingCharacters: EncodingChars
    sendingApplication: Field
    sendingFacility: Field
    receivingApplication: Field
    receivingFacility: Field
    dateTimeOfMessage: DateTime
    security: Field
    messageType: MessageType
    messageControlID: Field
    processingID: ProcessingID
    versionID: VersionID
    sequenceNumber: Field
    continuationPointer: Field
    acceptAcknowledgmentType: Field
    applicationAcknowledgmentType: Field
    countryCode: Field
    characterSet: Many CharSet
    principalLanguageOfMessage: Field

def EncodingChars =
  block
    componentSeparator: Char
    repetitionSeparator: Char
    escapeCharacter: Char
    subcomponentSeparator: Char

def MessageType =
  block
    messageCode: Field
    triggerEvent: Field
    messageStructure: Field

def ProcessingID =
  block
    processingID: Field
    processingMode: Field

def VersionID =
  block
    versionID: Field
    internationalizationCode: Field

def Segment =
  block
    segmentID: Field
    fields: Many Field

def Field =
  block
    components: Many Component

def Component =
  block
    subcomponents: Many Subcomponent

def Subcomponent =
  block
    value: String

def DateTime =
  block
    year: UInt16
    month: UInt8
    day: UInt8
    hour: UInt8
    minute: UInt8
    second: UInt8
    fractionalSecond: UInt8
    timeZone: TimeZone

def TimeZone =
  block
    offset: Int16

def CharSet =
  block
    charset: String

-- Entry point for parsing an HL7 v2.x message
def Main =
  HL7Message