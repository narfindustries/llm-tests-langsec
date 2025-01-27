-- HL7 v2.x Message Parser in Daedalus
-- This specification parses HL7 v2.x messages into their constituent segments and fields.

import Daedalus

-- Define the HL7 message structure
def HL7Message =
  block
    header: MSH
    segments: Many Segment

-- Define the MSH (Message Header) segment
def MSH =
  block
    fieldSeparator: Char
    encodingCharacters: EncodingChars
    sendingApplication: ST
    sendingFacility: ST
    receivingApplication: ST
    receivingFacility: ST
    dateTimeOfMessage: DTM
    security: ST
    messageType: MSG
    messageControlID: ST
    processingID: PT
    versionID: VID
    sequenceNumber: NM
    continuationPointer: ST
    acceptAcknowledgmentType: ID
    applicationAcknowledgmentType: ID
    countryCode: ID
    characterSet: ID
    principalLanguageOfMessage: CE
    alternateCharacterSetHandlingScheme: ID

-- Define the Encoding Characters
def EncodingChars =
  block
    componentSeparator: Char
    repetitionSeparator: Char
    escapeCharacter: Char
    subcomponentSeparator: Char

-- Define the Message Type (MSG)
def MSG =
  block
    messageCode: ID
    triggerEvent: ID
    messageStructure: ID

-- Define the Processing ID (PT)
def PT =
  block
    processingID: ID
    processingMode: ID

-- Define the Version ID (VID)
def VID =
  block
    versionID: ID
    internationalizationCode: ID
    internationalVersionID: ID

-- Define the Segment structure
def Segment =
  block
    segmentID: ST
    fields: Many Field

-- Define the Field structure
def Field =
  block
    components: Many Component

-- Define the Component structure
def Component =
  block
    subcomponents: Many Subcomponent

-- Define the Subcomponent structure
def Subcomponent =
  block
    value: ST

-- Define the ST (String) type
def ST = Many Char

-- Define the DTM (Date/Time) type
def DTM = ST

-- Define the CE (Coded Element) type
def CE =
  block
    identifier: ST
    text: ST
    nameOfCodingSystem: ST
    alternateIdentifier: ST
    alternateText: ST
    nameOfAlternateCodingSystem: ST

-- Define the ID (Identifier) type
def ID = ST

-- Define the NM (Numeric) type
def NM = ST

-- Define the Char type
def Char = UInt8

-- Define the Many combinator
def Many P = block
  count: UInt32
  elements: ManyP P count

-- Define the ManyP combinator
def ManyP P count =
  case count of
    0 -> block
    n -> block
      head: P
      tail: ManyP P (n - 1)