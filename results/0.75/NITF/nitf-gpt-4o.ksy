meta:
  id: nitf
  file-extension: nitf
  endian: be

seq:
  - id: file_header
    type: file_header

types:
  file_header:
    seq:
      - id: file_type
        type: str
        size: 4
      - id: version
        type: str
        size: 5
      - id: complexity
        type: str
        size: 1
      - id: system
        type: str
        size: 9
      - id: origin
        type: str
        size: 14
      - id: date_time
        type: str
        size: 14
      - id: title
        type: str
        size: 80
      - id: security
        type: security_info
      - id: file_size
        type: u8
      - id: header_size
        type: u4
      - id: num_image_segments
        type: u2
      - id: num_graphics_segments
        type: u2
      - id: num_text_segments
        type: u2
      - id: num_data_extension_segments
        type: u2
      - id: num_reserved_extension_segments
        type: u2

  security_info:
    seq:
      - id: classification
        type: str
        size: 1
      - id: system
        type: str
        size: 2
      - id: codewords
        type: str
        size: 11
      - id: control_number
        type: str
        size: 20
      - id: handling_description
        type: str
        size: 2
      - id: release_instructions
        type: str
        size: 20
      - id: declassification_type
        type: str
        size: 2
      - id: declassification_date
        type: str
        size: 8
      - id: declassification_exemption
        type: str
        size: 4
      - id: classification_reason
        type: str
        size: 1
      - id: owner_trigraph
        type: str
        size: 3
      - id: classification_source
        type: str
        size: 1
      - id: downgrading_description
        type: str
        size: 40
      - id: classification_text
        type: str
        size: 40
      - id: classification_authority_type
        type: str
        size: 1
      - id: classification_authority
        type: str
        size: 40
      - id: classification_authority_date
        type: str
        size: 8