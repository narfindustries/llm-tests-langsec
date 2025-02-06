meta:
  id: nitf
  endian: be
  docs: >
    This Kaitai Struct specification aims to represent the core structure of a NITF file
    as defined in MIL-STD-2500C.  Due to the complexity and extensibility of the standard,
    this specification is a simplified representation and may not cover all possible variations.
    Consult the official MIL-STD-2500C for complete details.  This spec focuses on the
    fundamental structure and common elements.  Many fields are represented generically
    due to the vast variety of options.

seq:
  - id: file_header
    type: nitf_file_header

  - id: image_segments
    type: seq
    repeat: eos
    - id: header
      type: nitf_image_header
    - id: data
      type: bytes
      size: lambda this: this.header.data_size


types:
  nitf_file_header:
    seq:
      - id: header_identifier
        type: str
        size: 12
      - id: file_version
        type: str
        size: 10
      - id: file_header_size
        type: u4
      - id: reserved_1
        type: bytes
        size: 2
      - id: image_header_file_size
        type: u4
      - id: reserved_2
        type: bytes
        size: 28
      - id: image_header_subheader
        type: nitf_image_header_subheader

  nitf_image_header_subheader:
    seq:
      - id: image_number
        type: str
        size: 10
      - id: image_date_time
        type: str
        size: 16
      - id: image_compression
        type: str
        size: 16 
      - id: image_resolution_x
        type: u4
      - id: image_resolution_y
        type: u4
      - id: spatial_reference_system
        type: str
        size: 255 
      - id: sensor_type
        type: str
        size: 255 
      - id: security_classification
        type: str
        size: 10 
      - id: data_size
        type: u4
      - id: tre_list
        type: nitf_tre_list


  nitf_tre_list:
    seq:
      - id: tre
        type: nitf_tre
        repeat: eos

  nitf_tre:
    seq:
      - id: tag
        type: str
        size: 4
      - id: length
        type: u4
      - id: data
        type: bytes
        size: lambda this: this.length

  nitf_image_header:
    seq:
      - id: header_identifier
        type: str
        size: 12
      - id: data_size
        type: u4
      - id: reserved
        type: bytes
        size: 2

