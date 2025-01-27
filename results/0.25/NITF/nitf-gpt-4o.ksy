meta:
  id: nitf
  title: NITF (National Imagery Transmission Format)
  file-extension: nitf
  endian: be

seq:
  - id: file_header
    type: file_header

types:
  file_header:
    seq:
      - id: file_type
        size: 4
        type: str
        encoding: ASCII
      - id: version
        size: 5
        type: str
        encoding: ASCII
      - id: complexity_level
        size: 2
        type: str
        encoding: ASCII
      - id: stype
        size: 4
        type: str
        encoding: ASCII
      - id: odate
        size: 14
        type: str
        encoding: ASCII
      - id: otime
        size: 14
        type: str
        encoding: ASCII
      - id: file_title
        size: 80
        type: str
        encoding: ASCII
      - id: file_security
        type: file_security

  file_security:
    seq:
      - id: classification
        size: 1
        type: str
        encoding: ASCII
      - id: classification_system
        size: 2
        type: str
        encoding: ASCII
      - id: codewords
        size: 11
        type: str
        encoding: ASCII
      - id: control_and_handling
        size: 2
        type: str
        encoding: ASCII
      - id: release_instructions
        size: 20
        type: str
        encoding: ASCII
      - id: declassification_type
        size: 2
        type: str
        encoding: ASCII
      - id: declassification_date
        size: 8
        type: str
        encoding: ASCII
      - id: declassification_exemption
        size: 4
        type: str
        encoding: ASCII
      - id: downgrade
        size: 1
        type: str
        encoding: ASCII
      - id: downgrade_date
        size: 8
        type: str
        encoding: ASCII
      - id: classification_text
        size: 43
        type: str
        encoding: ASCII
      - id: classification_authority_type
        size: 1
        type: str
        encoding: ASCII
      - id: classification_authority
        size: 40
        type: str
        encoding: ASCII
      - id: classification_reason
        size: 1
        type: str
        encoding: ASCII
      - id: security_source_date
        size: 8
        type: str
        encoding: ASCII
      - id: security_control_number
        size: 15
        type: str
        encoding: ASCII
      - id: security_control_number_2
        size: 15
        type: str
        encoding: ASCII