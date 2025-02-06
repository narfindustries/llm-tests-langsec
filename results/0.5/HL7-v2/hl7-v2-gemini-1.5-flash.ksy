endian: be

seq:
  - id: msh
    type: hl7_msh
  - id: segments
    type: seq
    repeat: true
    contents: any_segment

types:
  hl7_msh:
    type: struct
    fields:
      - id: field_separator
        type: u1
      - id: encoding_characters
        type: str
        size: 3
      - id: sending_application
        type: str
      - id: sending_facility
        type: str
      - id: receiving_application
        type: str
      - id: receiving_facility
        type: str
      - id: datetime
        type: str
      - id: security
        type: str
      - id: message_type
        type: str
      - id: message_control_id
        type: str
      - id: processing_id
        type: str
      - id: version_id
        type: str
      - id: sequence_number
        type: str
      - id: continuation_pointer
        type: str
      - id: accept_acknowledgement_type
        type: str
      - id: application_acknowledgement_type
        type: str
      - id: country_code
        type: str
      - id: character_set
        type: str

  any_segment:
    type: switch
    id: segment_type
    tag: type
    on:
      MSH:
        type: hl7_msh
      # Add other segment types here as needed (e.g., PID, PV1, etc.)
      # ... many more segment types ...  This is a placeholder; a real spec would require many more types.
      default:
        type: str

