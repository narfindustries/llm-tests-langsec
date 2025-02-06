# This is a simplified example and does not cover the entire HL7 v2 specification.
# A complete specification would be extremely large and complex.  This addresses the
# error by specifying the encoding for string types.  Assuming UTF-8 encoding.

types:
  hl7_message:
    seq:
      - id: msh
        type: msh_segment
      - id: segments
        type: repeat
        contents: any_segment

  msh_segment:
    seq:
      - id: field_separator
        type: char
      - id: encoding_characters
        type: str
        encoding: UTF-8
      - id: sending_application
        type: str
        encoding: UTF-8
      - id: sending_facility
        type: str
        encoding: UTF-8
      - id: receiving_application
        type: str
        encoding: UTF-8
      - id: receiving_facility
        type: str
        encoding: UTF-8
      - id: datetime
        type: str
        encoding: UTF-8
      - id: security
        type: str
        encoding: UTF-8
      - id: message_type
        type: str
        encoding: UTF-8
      - id: message_control_id
        type: str
        encoding: UTF-8
      - id: processing_id
        type: str
        encoding: UTF-8
      - id: version_id
        type: str
        encoding: UTF-8
      - id: sequence_number
        type: str
        encoding: UTF-8

  any_segment:
    seq:
      - id: segment_id
        type: str
        encoding: UTF-8
      - id: fields
        type: repeat
        contents: any_field

  any_field:
    id: field_value
    type: str
    encoding: UTF-8
