The error "mapping values are not allowed here" in Kaitai Struct usually means you've used a dictionary (mapping) where a sequence is expected, or vice-versa.  The previous attempts likely had issues with the `optional_fields` type.  Since the exact structure of optional fields is highly variable and dependent on the specific NITF file,  it's impossible to define them completely in a general Kaitai Struct.  A more practical approach is to treat them as a byte array of a known size.

Here's a revised YAML specification that addresses this by treating optional fields as a byte array.  Remember, this is still a simplified representation, and a fully compliant parser would require significantly more detail based on MIL-STD-2500C.


types:
  nitf_header:
    seq:
      - id: file_header
        type: nitf_file_header
      - id: image_header
        type: nitf_image_header
      - id: user_header
        type: bytes
        size: lambda: self.file_header.user_header_length
      - id: data_segments
        type: nitf_data_segment
        repeat: expr: self.file_header.num_trees

  nitf_file_header:
    seq:
      - id: header_length
        type: u4be
      - id: version
        type: strz
        size: 6
      - id: security_flags
        type: u4be
      - id: num_trees
        type: u4be
      - id: user_header_length
        type: u4be
      - id: optional_fields_length
        type: u4be
      - id: optional_fields
        type: bytes
        size: lambda: self.optional_fields_length


  nitf_image_header:
    seq:
      - id: num_images
        type: u4be
      - id: image_length
        type: u4be
      - id: image_width
        type: u4be
      - id: bit_per_pixel
        type: u4be
      - id: pixel_encoding
        type: strz
        size: 8
      - id: color_space
        type: strz
        size: 8
      - id: gsd
        type: f8be
      - id: image_orientation
        type: u4be
      - id: optional_fields_length
        type: u4be
      - id: optional_fields
        type: bytes
        size: lambda: self.optional_fields_length

  nitf_data_segment:
    seq:
      - id: tre_header
        type: nitf_tre_header
      - id: tre_data
        type: bytes
        size: lambda: self.tre_header.tre_length

  nitf_tre_header:
    seq:
      - id: tre_tag
        type: strz
        size: 4
      - id: tre_length
        type: u4be
      - id: optional_fields_length
        type: u4be
      - id: optional_fields
        type: bytes
        size: lambda: self.optional_fields_length

root: nitf_header
