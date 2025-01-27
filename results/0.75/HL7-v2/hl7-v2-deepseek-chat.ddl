-- HL7-v2 DeepSeek Chat Specification in Daedalus

import Daedalus

-- Define the HL7-v2 message structure
def HL7Message =
  block
    header: MSH
    segments: Many Segment

-- Define the MSH (Message Header) segment
def MSH =
  block
    fieldSeparator: Char
    encodingCharacters: EncodingChars
    sendingApplication: HD
    sendingFacility: HD
    receivingApplication: HD
    receivingFacility: HD
    dateTimeOfMessage: DTM
    security: ST?
    messageType: MSG
    messageControlID: ST
    processingID: PT
    versionID: VID
    sequenceNumber: NM?
    continuationPointer: ST?
    acceptAcknowledgmentType: ID?
    applicationAcknowledgmentType: ID?
    countryCode: ID?
    characterSet: ID?
    principalLanguageOfMessage: CE?
    alternateCharacterSetHandlingScheme: ID?
    messageProfileIdentifier: EI*

-- Define the Encoding Characters
def EncodingChars =
  block
    componentSeparator: Char
    repetitionSeparator: Char
    escapeCharacter: Char
    subcomponentSeparator: Char

-- Define the HD (Hierarchic Designator) type
def HD =
  block
    namespaceId: IS?
    universalId: ST?
    universalIdType: ID?

-- Define the DTM (Date/Time) type
def DTM =
  block
    time: DateTime

-- Define the MSG (Message Type) type
def MSG =
  block
    messageCode: ID
    triggerEvent: ID
    messageStructure: ID

-- Define the PT (Processing Type) type
def PT =
  block
    processingId: ID
    processingMode: ID

-- Define the VID (Version ID) type
def VID =
  block
    versionId: ID
    internationalizationCode: CE?
    internationalVersionId: CE?

-- Define the ST (String) type
def ST = String

-- Define the ID (Coded Value) type
def ID = String

-- Define the NM (Numeric) type
def NM = Number

-- Define the CE (Coded Element) type
def CE =
  block
    identifier: ST
    text: ST?
    nameOfCodingSystem: ST?
    alternateIdentifier: ST?
    alternateText: ST?
    nameOfAlternateCodingSystem: ST?

-- Define the EI (Entity Identifier) type
def EI =
  block
    entityIdentifier: ST
    namespaceId: IS?
    universalId: ST?
    universalIdType: ID?

-- Define the IS (Coded Value for User-Defined Tables) type
def IS = String

-- Define the Segment type
def Segment =
  block
    segmentId: ST
    fields: Many Field

-- Define the Field type
def Field =
  block
    fieldValue: ST
    components: Many Component

-- Define the Component type
def Component =
  block
    componentValue: ST
    subcomponents: Many Subcomponent

-- Define the Subcomponent type
def Subcomponent = ST

-- Define the DateTime type
def DateTime = String

-- Define the Number type
def Number = Int

-- Define the String type
def String = Many Char

-- Define the Char type
def Char = UInt8

-- Define the Many combinator
def Many P = block
  count: UInt32
  elements: P[count]

-- Define the UInt8 type
def UInt8 = UInt 8

-- Define the UInt32 type
def UInt32 = UInt 32

-- Define the UInt type
def UInt n = BitVector n