meta:
  id: hl7_v2_deepseek_chat
  title: HL7 v2 DeepSeek Chat
  file-extension: hl7
  encoding: UTF-8
  endian: be
  license: MIT
  ks-version: 0.9

seq:
  - id: message_header
    type: message_header
  - id: segments
    type: segment
    repeat: eos

types:
  message_header:
    seq:
      - id: field_separator
        type: str
        size: 1
      - id: encoding_characters
        type: str
        size: 4
      - id: sending_application
        type: str
        size: 180
      - id: sending_facility
        type: str
        size: 180
      - id: receiving_application
        type: str
        size: 180
      - id: receiving_facility
        type: str
        size: 180
      - id: date_time_of_message
        type: str
        size: 26
      - id: security
        type: str
        size: 40
      - id: message_type
        type: str
        size: 3
      - id: message_control_id
        type: str
        size: 20
      - id: processing_id
        type: str
        size: 3
      - id: version_id
        type: str
        size: 60
      - id: sequence_number
        type: str
        size: 15
      - id: continuation_pointer
        type: str
        size: 180
      - id: accept_acknowledgment_type
        type: str
        size: 2
      - id: application_acknowledgment_type
        type: str
        size: 2
      - id: country_code
        type: str
        size: 3
      - id: character_set
        type: str
        size: 16
      - id: principal_language_of_message
        type: str
        size: 3

  segment:
    seq:
      - id: segment_id
        type: str
        size: 3
      - id: fields
        type: field
        repeat: eos

  field:
    seq:
      - id: field_value
        type: str
        size-eos: true