-- HL7-v2 DeepSeek Chat Message Specification
-- Updated and fixed based on the error message

import Daedalus

-- Define the HL7-v2 message structure
def HL7Message =
  block
    header: MSHHeader
    segments: ManySegment

def MSHHeader =
  block
    fieldSeparator: $F
    encodingCharacters: $F
    sendingApplication: $F
    sendingFacility: $F
    receivingApplication: $F
    receivingFacility: $F
    dateTimeOfMessage: $F
    security: $F
    messageType: $F
    messageControlID: $F
    processingID: $F
    versionID: $F
    sequenceNumber: $F
    continuationPointer: $F
    acceptAcknowledgmentType: $F
    applicationAcknowledgmentType: $F
    countryCode: $F
    characterSet: $F
    principalLanguageOfMessage: $F

def ManySegment =
  block
    segment: Segment
    nextSegment: Optional ManySegment

def Segment =
  block
    segmentID: $F
    fields: ManyField

def ManyField =
  block
    field: Field
    nextField: Optional ManyField

def Field =
  block
    components: ManyComponent

def ManyComponent =
  block
    component: Component
    nextComponent: Optional ManyComponent

def Component =
  block
    subComponents: ManySubComponent

def ManySubComponent =
  block
    subComponent: SubComponent
    nextSubComponent: Optional ManySubComponent

def SubComponent =
  block
    value: $F

-- Define the field separator and encoding characters
def $F =
  block
    value: String

-- Entry point for parsing HL7-v2 messages
def main =
  HL7Message