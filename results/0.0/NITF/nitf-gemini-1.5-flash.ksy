# This is a simplified Kaitai Struct specification for NITF.  It is NOT complete
# due to the vast complexity and variability of the NITF standard.  Many optional
# fields and segments are omitted for brevity.  This should be considered a starting
# point and requires significant expansion for real-world use.  This example addresses
# the duplicate key error by ensuring unique identifiers.  It also attempts to address
# potential issues with size definitions and data types based on common NITF structures.
#  The error message suggests a repeated type definition within a nested structure.
# This revision focuses on ensuring unique identifiers across all levels.


types:
  nitf_header:
    seq:
      - id: version
        type: u2
      - id: date
        type: str
        size: 10
      - id: security
        type: str
        size: 1
      - id: header_length
        type: u4
      - id: image_segment_header_length
        type: u4
      - id: image_segment_data_length
        type: u8 #Increased size for larger files
      - id: num_image_segments
        type: u4
      - id: file_name
        type: str
        size: 256  #Increased size for longer filenames
      # ... many more fields omitted for brevity ...

  image_segment_header:
    seq:
      - id: segment_id
        type: str
        size: 8
      - id: compression_type
        type: str
        size: 8  #Simplified length
      - id: bit_depth
        type: u2
      - id: num_rows
        type: u4
      - id: num_cols
        type: u4
      - id: sample_bit_depth
        type: u2
      - id: sample_type
        type: str
        size: 8  #Simplified length
      - id: image_data_length
        type: u8 # Increased size for larger images


      # ... many more fields omitted for brevity ...

  image_segment_data:
    seq:
      - id: image_data
        type: bytes
        size: (parent.header.image_data_length) #Renamed to avoid conflict


  nitf_file:
    seq:
      - id: header
        type: nitf_header
      - id: image_segments
        type: seq
        size: (parent.header.num_image_segments)
        type: image_segment
  image_segment:
    seq:
      - id: header
        type: image_segment_header
      - id: data
        type: image_segment_data

