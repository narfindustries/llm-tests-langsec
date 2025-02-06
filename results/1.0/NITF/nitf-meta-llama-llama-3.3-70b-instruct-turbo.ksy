meta:
  id: nitf
  endian: be
seq:
  - id: file_header_length
    type: u4
  - id: file_header_version
    type: u2
  - id: file_type
    type: str
    encoding: ascii
    size: 25
    pad_byte: 0
  - id: file_format
    type: str
    encoding: ascii
    size: 25
    pad_byte: 0
  - id: file_security_classification
    type: u1
  - id: file_control_number
    type: str
    encoding: ascii
    size: 25
    pad_byte: 0
  - id: file_header_extension
    type: file_header_extension
    if: file_header_version >= 2
  - id: image_header_length
    type: u4
  - id: image_id
    type: str
    encoding: ascii
    size: 25
    pad_byte: 0
  - id: image_date_and_time
    type: str
    encoding: ascii
    size: 14
    pad_byte: 0
  - id: image_security_classification
    type: u1
  - id: image_title
    type: str
    encoding: ascii
    size: 80
    pad_byte: 0
  - id: image_source
    type: str
    encoding: ascii
    size: 42
    pad_byte: 0
  - id: image_coordinate_system
    type: u1
  - id: image_reference_point
    type: u4
    repeat: 3
  - id: image_orientation
    type: u4
    repeat: 3
  - id: image_header_extension
    type: image_header_extension
    if: file_header_version >= 2
  - id: band_headers
    type: band_header
    repeat: expr
    expr: file_header_length - 154
  - id: image_data
    type: u1
    size: expr
    expr: image_header_length - 512
types:
  file_header_extension:
    seq:
      - id: extension_header_length
        type: u4
      - id: extension_id
        type: str
        encoding: ascii
        size: 25
        pad_byte: 0
      - id: extension_data
        type: u1
        size: expr
        expr: extension_header_length - 64
  image_header_extension:
    seq:
      - id: extension_header_length
        type: u4
      - id: extension_id
        type: str
        encoding: ascii
        size: 25
        pad_byte: 0
      - id: extension_data
        type: u1
        size: expr
        expr: extension_header_length - 64
  band_header:
    seq:
      - id: band_header_length
        type: u4
      - id: band_id
        type: str
        encoding: ascii
        size: 25
        pad_byte: 0
      - id: band_number
        type: u2
      - id: band_representation
        type: u1
      - id: band_data_type
        type: u1
      - id: band_size
        type: u4
        repeat: 2
      - id: band_data
        type: u1
        size: expr
        expr: band_header_length - 128