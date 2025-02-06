meta:
  id: nitf
  title: National Imagery Transmission Format (NITF)
  file-extension: nitf
  endian: be
seq:
  - id: header
    type: file_header
  - id: image_segments
    type: image_segment
    repeat: expr
    repeat-expr: header.num_image_segments
  - id: graphics_segments
    type: graphics_segment
    repeat: expr
    repeat-expr: header.num_graphics_segments
  - id: text_segments
    type: text_segment
    repeat: expr
    repeat-expr: header.num_text_files
  - id: data_extension_segments
    type: data_extension_segment
    repeat: expr
    repeat-expr: header.num_data_extension
  - id: reserved_extension_segments
    type: reserved_extension_segment
    repeat: expr
    repeat-expr: header.num_reserved_extension
types:
  file_header:
    seq:
      - id: file_profile_name
        type: str
        size: 4
        encoding: ASCII
      - id: file_version
        type: str
        size: 5
        encoding: ASCII
      - id: complexity_level
        type: str
        size: 2
        encoding: ASCII
      - id: standard_type
        type: str
        size: 4
        encoding: ASCII
      - id: originating_station_id
        type: str
        size: 10
        encoding: ASCII
      - id: file_date_time
        type: str
        size: 14
        encoding: ASCII
      - id: file_title
        type: str
        size: 80
        encoding: ASCII
      - id: file_security_classification
        type: str
        size: 1
        encoding: ASCII
      - id: file_copy_number
        type: str
        size: 5
        encoding: ASCII
      - id: file_num_of_copies
        type: str
        size: 5
        encoding: ASCII
      - id: encryption
        type: u1
      - id: file_bg_color
        type: u3
      - id: originator_name
        type: str
        size: 24
        encoding: ASCII
      - id: originator_phone
        type: str
        size: 18
        encoding: ASCII
      - id: file_length
        type: str
        size: 12
        encoding: ASCII
      - id: header_length
        type: str
        size: 6
        encoding: ASCII
      - id: num_image_segments
        type: u2
      - id: linfo
        type: length_image_info
        repeat: expr
        repeat-expr: num_image_segments
      - id: num_graphics_segments
        type: u2
      - id: num_text_files
        type: u2
      - id: num_data_extension
        type: u2
      - id: num_reserved_extension
        type: u2
  length_image_info:
    seq:
      - id: length_image_subheader
        type: u4
      - id: length_image_data
        type: u4
  image_segment:
    seq:
      - id: image_sub_header
        type: image_sub_header
      - id: image_data_field
        size-eos: true
  image_sub_header:
    seq:
      - id: iid1
        type: str
        size: 10
        encoding: ASCII
      - id: idatim
        type: str
        size: 14
        encoding: ASCII
      - id: tgtid
        type: str
        size: 17
        encoding: ASCII
      - id: iid2
        type: str
        size: 80
        encoding: ASCII
      - id: isclas
        type: str
        size: 1
        encoding: ASCII
      - id: nrows
        type: u4
      - id: ncols
        type: u4
      - id: pvtype
        type: str
        size: 3
        encoding: ASCII
      - id: irep
        type: str
        size: 8
        encoding: ASCII
      - id: icat
        type: str
        size: 8
        encoding: ASCII
      - id: abpp
        type: u2
      - id: pjust
        type: str
        size: 1
        encoding: ASCII
      - id: icords
        type: str
        size: 1
        encoding: ASCII
      - id: ic
        type: str
        size: 2
        encoding: ASCII
  graphics_segment:
    seq:
      - id: graphics_sub_header
        type: graphics_sub_header
      - id: graphics_data_field
        size-eos: true
  graphics_sub_header:
    seq:
      - id: sid
        type: str
        size: 10
        encoding: ASCII
      - id: sname
        type: str
        size: 20
        encoding: ASCII
      - id: sclas
        type: str
        size: 1
        encoding: ASCII
  text_segment:
    seq:
      - id: text_sub_header
        type: text_sub_header
      - id: text_data_field
        size-eos: true
  text_sub_header:
    seq:
      - id: textid
        type: str
        size: 7
        encoding: ASCII
      - id: txtalvl
        type: u1
      - id: tsclas
        type: str
        size: 1
        encoding: ASCII
  data_extension_segment:
    seq:
      - id: data_sub_header
        type: data_sub_header
      - id: data_data_field
        size-eos: true
  data_sub_header:
    seq:
      - id: desid
        type: str
        size: 25
        encoding: ASCII
      - id: desver
        type: u2
      - id: declas
        type: str
        size: 1
        encoding: ASCII
  reserved_extension_segment:
    seq:
      - id: reserved_sub_header
        type: reserved_sub_header
      - id: reserved_data_field
        size-eos: true
  reserved_sub_header:
    seq:
      - id: res_type
        type: str
        size: 25
        encoding: ASCII
      - id: resver
        type: u2
      - id: resclas
        type: str
        size: 1
        encoding: ASCII