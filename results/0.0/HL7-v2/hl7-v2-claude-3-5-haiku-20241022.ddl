module HL7-v2

import Daedalus.Core
import Daedalus.Encoding.ASCII

type MSH = {
  field_separator: Char,
  encoding_characters: String(4),
  sending_application: String,
  sending_facility: String,
  receiving_application: String,
  receiving_facility: String,
  timestamp: DateTime,
  security: Maybe String,
  message_type: String,
  message_control_id: String,
  processing_id: String,
  version: String,
  sequence_number: Maybe Integer,
  continuation_pointer: Maybe String,
  accepting_acknowledgment_type: Maybe String,
  application_acknowledgment_type: Maybe String,
  country_code: Maybe String
}

type Segment = 
  | MSHSegment MSH
  | OtherSegment String

type HL7Message = {
  header: MSH,
  segments: List Segment
}

parser msh_segment: MSH = 
  do
    char '|'
    encoding_chars <- take 4
    field_separator <- char
    sending_app <- field
    sending_facility <- field 
    receiving_app <- field
    receiving_facility <- field
    timestamp <- datetime
    security <- optional field
    message_type <- field
    message_control_id <- field
    processing_id <- field
    version <- field
    sequence_num <- optional integer
    continuation_ptr <- optional field
    ack_type1 <- optional field
    ack_type2 <- optional field
    country_code <- optional field
    
    return {
      field_separator,
      encoding_characters = encoding_chars,
      sending_application = sending_app,
      sending_facility,
      receiving_application = receiving_app, 
      receiving_facility,
      timestamp,
      security,
      message_type,
      message_control_id,
      processing_id,
      version,
      sequence_number = sequence_num,
      continuation_pointer = continuation_ptr,
      accepting_acknowledgment_type = ack_type1,
      application_acknowledgment_type = ack_type2,
      country_code
    }

parser hl7_message: HL7Message =
  do
    header <- msh_segment
    segments <- many (
      choice [
        MSHSegment <$> msh_segment,
        OtherSegment <$> segment_line
      ]
    )
    return { header, segments }

parser segment_line: String =
  takeWhile (\c -> c != '\r' && c != '\n')

main = hl7_message