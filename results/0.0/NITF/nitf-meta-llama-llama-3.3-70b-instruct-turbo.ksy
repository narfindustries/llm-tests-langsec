seq:
  - id: file_header
    type: file_header
  - id: image_segments
    type: image_segment
    repeat: expr
    repeat-expr: file_header.num_image_segments
  - id: graphic_segments
    type: graphic_segment
    repeat: expr
    repeat-expr: file_header.num_graphic_segments
  - id: text_segments
    type: text_segment
    repeat: expr
    repeat-expr: file_header.num_text_segments
  - id: trailer
    type: trailer

types:
  file_header:
    seq:
      - id: file_header_identifier
        size: 9
      - id: file_version
        size: 5
      - id: classification_level
        size: 1
      - id: classification_system
        size: 2
      - id: classification_code
        size: 11
      - id: release_instructions
        size: 25
      - id: control_and_handling
        size: 11
      - id: num_standards
        size: 2
      - id: originator_standard_identifier
        size: 25
      - id: originator_title
        size: 20
      - id: originator_name
        size: 24
      - id: originator_address
        size: 42
      - id: file_date_and_time
        size: 14
      - id: file_title
        size: 80
      - id: file_security_classification
        size: 1
      - id: num_image_segments
        size: 3
      - id: num_graphic_segments
        size: 3
      - id: num_text_segments
        size: 3
      - id: reserved
        size: 20
    enums:
      classification_level:
        - U: "Unclassified"
        - C: "Confidential"
        - S: "Secret"
        - T: "Top Secret"
      classification_system:
        - US: "US Classification System"
        - CA: "Canadian Classification System"
        - GB: "British Classification System"
        - FR: "French Classification System"
      file_version:
        - "02.10": "NITF 02.10"
        - "02.11": "NITF 02.11"
      file_security_classification:
        - U: "Unclassified"
        - C: "Confidential"
        - S: "Secret"
        - T: "Top Secret"

  image_segment:
    seq:
      - id: image_segment_identifier
        size: 2
      - id: image_identifier_1
        size: 25
      - id: image_identifier_2
        size: 17
      - id: image_date_and_time
        size: 14
      - id: image_type
        size: 3
      - id: num_rows
        size: 6
      - id: num_columns
        size: 6
      - id: image_representation
        size: 2
      - id: num_bands
        size: 1
      - id: image_compression
        size: 2
      - id: encryption
        size: 1
      - id: reserved
        size: 13
      - id: image_data
        size: expr
        size-expr: num_rows * num_columns * num_bands
    enums:
      image_type:
        - VIS: "Visible"
        - IR: "Infrared"
        - UV: "Ultraviolet"
        - MM: "Multispectral"
      image_representation:
        - MONO: "Monochrome"
        - RGB: "RGB Color"
        - YCbCr: "YCbCr Color"
      image_compression:
        - NONE: "No Compression"
        - JPEG: "JPEG Compression"
        - CCITT: "CCITT Compression"

  graphic_segment:
    seq:
      - id: graphic_segment_identifier
        size: 2
      - id: graphic_identifier_1
        size: 25
      - id: graphic_identifier_2
        size: 17
      - id: graphic_date_and_time
        size: 14
      - id: graphic_type
        size: 3
      - id: num_points
        size: 6
      - id: graphic_data
        size: expr
        size-expr: num_points * 2
    enums:
      graphic_type:
        - LINE: "Line"
        - POLY: "Polygon"
        - CIRC: "Circle"

  text_segment:
    seq:
      - id: text_segment_identifier
        size: 2
      - id: text_identifier_1
        size: 25
      - id: text_identifier_2
        size: 17
      - id: text_date_and_time
        size: 14
      - id: text_type
        size: 3
      - id: text_length
        size: 6
      - id: text_data
        size: expr
        size-expr: text_length
    enums:
      text_type:
        - ASCII: "ASCII Text"
        - UTF8: "UTF-8 Text"

  trailer:
    seq:
      - id: trailer_identifier
        size: 2
      - id: trailer_length
        size: 6
      - id: trailer_data
        size: expr
        size-expr: trailer_length