meta:
  id: nitf
  file-extension: nitf
  endian: big
  title: National Image Transmission Format
  license: MIT

types:
  u4le: u4

seq:
  - id: header
    type: nitf_header

  - id: image_segments
    type: image_segment
    repeat: expr
    repeat-expr: header.num_image_segments

  - id: graphic_segments
    type: graphic_segment
    repeat: expr
    repeat-expr: header.num_graphic_segments

  - id: text_segments
    type: text_segment
    repeat: expr
    repeat-expr: header.num_text_segments

  - id: data_extension_segments
    type: data_extension_segment
    repeat: expr
    repeat-expr: header.num_data_extension_segments

types:
  nitf_header:
    seq:
      - id: file_header
        type: str
        size: 4
        enum: [NITF, NITF01, NITF02]

      - id: file_version
        type: str
        size: 5

      - id: file_level
        type: str
        size: 2

      - id: file_title
        type: str
        size: 24

      - id: file_security_classification
        type: str
        size: 1

      - id: file_copy_notification
        type: str
        size: 1

      - id: file_control_number
        type: str
        size: 20

      - id: file_date
        type: str
        size: 6

      - id: file_time
        type: str
        size: 4

      - id: file_title_extension
        type: str
        size: 47

      - id: file_security_classification_title
        type: str
        size: 1

      - id: file_security_classification_title extension
        type: str
        size: 11

      - id: file_control_number_title_extension
        type: str
        size: 15

      - id: file_length
        type: u4

      - id: header_length
        type: u4

      - id: num_image_segments
        type: u2

      - id: num_graphic_segments
        type: u2

      - id: num_text_segments
        type: u2

      - id: num_data_extension_segments
        type: u2

      - id: reserved
        type: str
        size: 20

  image_segment:
    seq:
      - id: image_header
        type: image_header

      - id: image_data
        type: str
        size: image_header.image_length

  graphic_segment:
    seq:
      - id: graphic_header
        type: graphic_header

      - id: graphic_data
        type: str
        size: graphic_header.graphic_length

  text_segment:
    seq:
      - id: text_header
        type: text_header

      - id: text_data
        type: str
        size: text_header.text_length

  data_extension_segment:
    seq:
      - id: data_extension_header
        type: data_extension_header

      - id: data_extension_data
        type: str
        size: data_extension_header.data_extension_length

  image_header:
    seq:
      - id: image_id
        type: str
        size: 25

      - id: image_date
        type: str
        size: 6

      - id: image_time
        type: str
        size: 4

      - id: image_title
        type: str
        size: 20

      - id: image_security_classification
        type: str
        size: 1

      - id: image_assigned_security_classification
        type: str
        size: 1

      - id: image_releasability_to
        type: str
        size: 20

      - id: image_releasability_to περιο
        type: str
        size: 20

      - id: image_control_number
        type: str
        size: 20

      - id: image_message_id
        type: str
        size: 5

      - id: image_sources
        type: str
        size: 42

      - id: image_processing_code
        type: str
        size: 2

      - id: image_scaled_height
        type: u4

      - id: image_scaled_width
        type: u4

      - id: image_pixel_type
        type: str
        size: 3

      - id: image_pixel_values
        type: u4

      - id: image_image_representation
        type: str
        size: 2

      - id: image_image_category
        type: str
        size: 8

      - id: image_angular_extent
        type: u4

      - id: image_angular_precision
        type: u4

      - id: image addItem_type
        type: str
        size: 4

      - id: image_item_category
        type: str
        size: 6

      - id: image_item_height
        type: u4

      - id: image_item_width
        type: u4

      - id: image_item_data_type
        type: str
        size: 3

      - id: image_item_data_values
        type: u4

      - id: image_item_representation
        type: str
        size: 2

      - id: image_item_category_extension
        type: str
        size: 12

      - id: image_item panties
        type: str
        size: 15

      - id: image_length
        type: u8

      - id: image_header_length
        type: u4

  graphic_header:
    seq:
      - id: graphic_id
        type: str
        size: 25

      - id: graphic_date
        type: str
        size: 6

      - id: graphic_time
        type: str
        size: 4

      - id: graphic_title
        type: str
        size: 20

      - id: graphic_security_classification
        type: str
        size: 1

      - id: graphic_assigned_security_classification
        type: str
        size: 1

      - id: graphic_releasability_to
        type: str
        size: 20

      - id: graphic_releasability_to інші
        type: str
        size: 20

      - id: graphic_control_number
        type: str
        size: 20

      - id: graphic_message_id
        type: str
        size: 5

      - id: graphic_sources
        type: str
        size: 42

      - id: graphic_processing_code
        type: str
        size: 2

      - id: graphic_scaled_height
        type: u4

      - id: graphic_scaled_width
        type: u4

      - id: graphic_pixel_type
        type: str
        size: 3

      - id: graphic_pixel_values
        type: u4

      - id: graphic_image_representation
        type: str
        size: 2

      - id: graphic_image_category
        type: str
        size: 8

      - id: graphic_angular_extent
        type: u4

      - id: graphic_angular_precision
        type: u4

      - id: graphic_item_type
        type: str
        size: 4

      - id: graphic_item_category
        type: str
        size: 6

      - id: graphic_item_height
        type: u4

      - id: graphic_item_width
        type: u4

      - id: graphic_item_data_type
        type: str
        size: 3

      - id: graphic_item_data_values
        type: u4

      - id: graphic_item_representation
        type: str
        size: 2

      - id: graphic_item_category_extension
        type: str
        size: 12

      - id: graphic_item_gottes
        type: str
        size: 15

      - id: graphic_length
        type: u8

      - id: graphic_header_length
        type: u4

  text_header:
    seq:
      - id: text_id
        type: str
        size: 25

      - id: text_date
        type: str
        size: 6

      - id: text_time
        type: str
        size: 4

      - id: text_title
        type: str
        size: 20

      - id: text_security_classification
        type: str
        size: 1

      - id: text_assigned_security_classification
        type: str
        size: 1

      - id: text_releasability_to
        type: str
        size: 20

      - id: text_releasability_to (),
        type: str
        size: 20

      - id: text_control_number
        type: str
        size: 20

      - id: text_message_id
        type: str
        size: 5

      - id: text_sources
        type: str
        size: 42

      - id: text_processing_code
        type: str
        size: 2

      - id: text_text_data_type
        type: str
        size: 3

      - id: text_text_data_values
        type: u4

      - id: text_item_type
        type: str
        size: 4

      - id: text_item_category
        type: str
        size: 6

      - id: text_item_length
        type: u4

      - id: text_item_header_length
        type: u4

      - id: text_length
        type: u8

      - id: text_header_length
        type: u4

  data_extension_header:
    seq:
      - id: data_extension_id
        type: str
        size: 25

      - id: data_extension_date
        type: str
        size: 6

      - id: data_extension_time
        type: str
        size: 4

      - id: data_extension_title
        type: str
        size: 20

      - id: data_extension_security_classification
        type: str
        size: 1

      - id: data_extension_assigned_security_classification
        type: str
        size: 1

      - id: data_extension_releasability_to
        type: str
        size: 20

      - id: data_extension_releasability_toКИ
        type: str
        size: 20

      - id: data_extension_control_number
        type: str
        size: 20

      - id: data_extension_message_id
        type: str
        size: 5

      - id: data_extension_sources
        type: str
        size: 42

      - id: data_extension_processing_code
        type: str
        size: 2

      - id: data_extension_data_type
        type: str
        size: 3

      - id: data_extension_data_values
        type: u4

      - id: data_extension_item_type
        type: str
        size: 4

      - id: data_extension_item_category
        type: str
        size: 6

      - id: data_extension_item_length
        type: u4

      - id: data_extension_item_header_length
        type: u4

      - id: data_extension_length
        type: u8

      - id: data_extension_header_length
        type: u4