types:
  nitf_header:
    seq:
      - id: file_header
        type: nitf_file_header
      - id: image_header
        type: nitf_image_header
      - id: data_segments
        type: seq
        items: nitf_data_segment
  nitf_file_header:
    seq:
      - id: prefix
        type: str
        len: 4
        encoding: ASCII
      - id: version
        type: str
        len: 4
        encoding: ASCII
      - id: file_name
        type: str
        len: 10
        encoding: ASCII
      - id: security_classification
        type: str
        len: 1
        encoding: ASCII
      - id: image_header_offset
        type: u4le
      - id: num_data_segments
        type: u4le
      - id: optional_fields
        type: repeat_until
        until_byte: 0x00000000
        items: nitf_optional_field
  nitf_image_header:
    seq:
      - id: prefix
        type: str
        len: 4
        encoding: ASCII
      - id: image_width
        type: u4le
      - id: image_height
        type: u4le
      - id: num_bands
        type: u2le
      - id: pixel_type
        type: u2le
      - id: compression_type
        type: u2le
      - id: optional_fields
        type: repeat_until
        until_byte: 0x00000000
        items: nitf_optional_field
  nitf_data_segment:
    seq:
      - id: segment_header
        type: nitf_segment_header
      - id: segment_data
        type: bytes
        len: (self.segment_header.segment_length - 16)
  nitf_segment_header:
    seq:
      - id: prefix
        type: str
        len: 4
        encoding: ASCII
      - id: segment_type
        type: str
        len: 4
        encoding: ASCII
      - id: segment_length
        type: u4le
      - id: optional_fields
        type: repeat_until
        until_byte: 0x00000000
        items: nitf_optional_field
  nitf_optional_field:
    seq:
      - id: tag
        type: str
        len: 4
        encoding: ASCII
      - id: length
        type: u4le
      - id: value
        type: bytes
        len: this.length

