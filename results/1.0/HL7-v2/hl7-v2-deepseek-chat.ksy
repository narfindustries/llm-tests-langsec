meta:
  id: hl7_v2_deepseek_chat
  title: HL7 v2 DeepSeek Chat Message Format
  file-extension: hl7
  encoding: UTF-8
  endian: be
  license: MIT
  ks-version: 0.9

doc: |
  HL7 v2 DeepSeek Chat Message Format specification for Kaitai Struct.
  This format is used to parse HL7 v2 messages with a focus on DeepSeek Chat.

seq:
  - id: message_header
    type: msh_segment
    doc: MSH segment containing the message header.

  - id: segments
    type: segment
    repeat: eos
    doc: List of segments in the HL7 v2 message.

types:
  msh_segment:
    seq:
      - id: field_separator
        type: str
        size: 1
        doc: Field separator character.
      - id: encoding_characters
        type: str
        size: 4
        doc: Encoding characters.
      - id: sending_application
        type: str
        size: 180
        doc: Sending application.
      - id: sending_facility
        type: str
        size: 180
        doc: Sending facility.
      - id: receiving_application
        type: str
        size: 180
        doc: Receiving application.
      - id: receiving_facility
        type: str
        size: 180
        doc: Receiving facility.
      - id: date_time_of_message
        type: str
        size: 26
        doc: Date/time of the message.
      - id: security
        type: str
        size: 40
        doc: Security field.
      - id: message_type
        type: str
        size: 7
        doc: Message type.
      - id: message_control_id
        type: str
        size: 20
        doc: Message control ID.
      - id: processing_id
        type: str
        size: 3
        doc: Processing ID.
      - id: version_id
        type: str
        size: 60
        doc: Version ID.
      - id: sequence_number
        type: str
        size: 15
        doc: Sequence number.
      - id: continuation_pointer
        type: str
        size: 180
        doc: Continuation pointer.
      - id: accept_acknowledgment_type
        type: str
        size: 2
        doc: Accept acknowledgment type.
      - id: application_acknowledgment_type
        type: str
        size: 2
        doc: Application acknowledgment type.
      - id: country_code
        type: str
        size: 3
        doc: Country code.
      - id: character_set
        type: str
        size: 16
        doc: Character set.
      - id: principal_language_of_message
        type: str
        size: 3
        doc: Principal language of the message.
      - id: alternate_character_set_handling_scheme
        type: str
        size: 20
        doc: Alternate character set handling scheme.
      - id: message_profile_identifier
        type: str
        size: 427
        doc: Message profile identifier.

  segment:
    seq:
      - id: segment_id
        type: str
        size: 3
        doc: Segment ID.
      - id: fields
        type: field
        repeat: eos
        doc: List of fields in the segment.

  field:
    seq:
      - id: field_value
        type: str
        size-eos: true
        doc: Field value.