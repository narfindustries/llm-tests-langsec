meta:
  id: nitf
  title: National Imagery Transmission Format
  file-extension: [ntf, nitf]
  encoding: ascii
  endian: be

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
  - id: data_extension_segments
    type: des_segment
    repeat: expr
    repeat-expr: file_header.num_data_extension_segments
  - id: reserved_extension_segments
    type: res_segment
    repeat: expr
    repeat-expr: file_header.num_reserved_extension_segments

types:
  file_header:
    seq:
      - id: fhdr
        contents: "NITF"
      - id: fver
        type: strz
        size: 5
      - id: clevel
        type: u1
      - id: stype
        type: security_type
      - id: orig
        type: strz
        size: 10
      - id: fdt
        type: strz
        size: 14
      - id: ftitle
        type: strz
        size: 80
      - id: fscop
        type: u2
      - id: fscpys
        type: u2
      - id: encryption
        type: strz
        size: 1
      - id: num_image_segments
        type: u4
      - id: num_graphic_segments
        type: u4
      - id: num_text_segments
        type: u4
      - id: num_data_extension_segments
        type: u4
      - id: num_reserved_extension_segments
        type: u4
      - id: user_header_length
        type: u4
      - id: extended_header_length
        type: u4

  security_type:
    seq:
      - id: security_classification
        type: strz
        size: 1
      - id: security_country_code
        type: strz
        size: 2
      - id: security_release_marking
        type: strz
        size: 6

  image_segment:
    seq:
      - id: image_subheader
        type: image_subheader
      - id: image_data
        size-eos: true

  image_subheader:
    seq:
      - id: im
        contents: "IM"
      - id: image_security
        type: security_type
      - id: encrypted
        type: u1
      - id: image_source
        type: strz
        size: 42
      - id: number_of_significant_rows
        type: u4
      - id: number_of_significant_columns
        type: u4
      - id: pixel_type
        type: strz
        size: 3
      - id: image_representation
        type: strz
        size: 8
      - id: image_category
        type: strz
        size: 3

  graphic_segment:
    seq:
      - id: graphic_subheader
        type: graphic_subheader
      - id: graphic_data
        size-eos: true

  graphic_subheader:
    seq:
      - id: gs
        contents: "GS"
      - id: graphic_security
        type: security_type

  text_segment:
    seq:
      - id: text_subheader
        type: text_subheader
      - id: text_data
        size-eos: true

  text_subheader:
    seq:
      - id: tx
        contents: "TX"
      - id: text_security
        type: security_type

  des_segment:
    seq:
      - id: des_subheader
        type: des_subheader
      - id: des_data
        size-eos: true

  des_subheader:
    seq:
      - id: de
        contents: "DE"
      - id: des_security
        type: security_type

  res_segment:
    seq:
      - id: res_subheader
        type: res_subheader
      - id: res_data
        size-eos: true

  res_subheader:
    seq:
      - id: re
        contents: "RE"
      - id: res_security
        type: security_type