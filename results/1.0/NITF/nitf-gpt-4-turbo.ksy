meta:
  id: nitf
  title: National Imagery Transmission Format (NITF)
  endian: be

seq:
  - id: header
    type: file_header
  - id: image_segments
    type: image_segment
    repeat: expr
    repeat-expr: header.numi
  - id: graphic_segments
    type: graphic_segment
    repeat: expr
    repeat-expr: header.numg
  - id: text_segments
    type: text_segment
    repeat: expr
    repeat-expr: header.numt
  - id: data_extension_segments
    type: data_extension_segment
    repeat: expr
    repeat-expr: header.numdes
  - id: reserved_extension_segments
    type: reserved_extension_segment
    repeat: expr
    repeat-expr: header.numres

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
      - id: complex_level
        type: u1
      - id: system_type
        type: str
        size: 4
        encoding: ASCII
      - id: originating_station_id
        type: str
        size: 10
        encoding: ASCII
      - id: file_datetime
        type: str
        size: 14
        encoding: ASCII
      - id: file_title
        type: str
        size: 80
        encoding: ASCII
      - id: file_security_class
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
        type: u8
      - id: header_length
        type: u4
      - id: numi
        type: u2
      - id: linfo
        type: location_info
        repeat: expr
        repeat-expr: numi
      - id: numg
        type: u2
      - id: numt
        type: u3
      - id: numdes
        type: u2
      - id: numres
        type: u2

  location_info:
    seq:
      - id: length_image_subheader
        type: u4
      - id: image_subheader_offset
        type: u4
      - id: length_image
        type: u8
      - id: image_offset
        type: u8
        
  image_segment:
    seq:
      - id: image_subheader
        type: image_subheader
      - id: image_data
        size: _parent._.linfo[_index].length_image

  image_subheader:
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
      - id: encrypt
        type: u1
      - id: isorce
        type: str
        size: 42
        encoding: ASCII
      - id: nlat
        type: u2
      - id: isctln
        type: str
        size: 15
        encoding: ASCII
      - id: isctlh
        type: str
        size: 40
        encoding: ASCII
      - id: isrel
        type: str
        size: 40
        encoding: ASCII
      - id: iscnsp
        type: str
        size: 20
        encoding: ASCII
      - id: iscod
        type: str
        size: 11
        encoding: ASCII
      - id: isctl
        type: str
        size: 2
        encoding: ASCII
      - id: isdctp
        type: str
        size: 2
        encoding: ASCII
      - id: isdcdt
        type: str
        size: 8
        encoding: ASCII
      - id: isdcxm
        type: str
        size: 4
        encoding: ASCII
      - id: isdg
        type: str
        size: 1
        encoding: ASCII
      - id: isdct
        type: str
        size: 40
        encoding: ASCII
      - id: islw
        type: u4
      - id: isln
        type: u4
      - id: nbands
        type: u1
      - id: xbands
        type: u2
      - id: icat
        type: str
        size: 8
        encoding: ASCII
      - id: abpp
        type: u2
      - id: pvtype
        type: str
        size: 3
        encoding: ASCII
      - id: irep
        type: str
        size: 8
        encoding: ASCII
      - id: icom
        type: str
        size: 1
        encoding: ASCII
      - id: ic
        type: str
        size: 2
        encoding: ASCII
      - id: width
        type: u4
      - id: height
        type: u4

  graphic_segment:
    seq:
      - id: subheader
        type: graphic_subheader
      - id: graphic_data
        size: _parent.header.file_length - _parent.header.header_length

  graphic_subheader:
    seq:
      - id: sid
        type: str
        size: 10
        encoding: ASCII
      - id: sname
        type: str
        size: 20
        encoding: ASCII
      - id: sclass
        type: str
        size: 1
        encoding: ASCII
      - id: sscsys
        type: str
        size: 2
        encoding: ASCII
      - id: sencrypt
        type: u1
      - id: sfmt
        type: str
        size: 3
        encoding: ASCII
      - id: sl
        type: u4
      - id: sw
        type: u4

  text_segment:
    seq:
      - id: text_subheader
        type: text_subheader
      - id: text_data
        type: str
        size-eos: true
        encoding: ASCII
        
  text_subheader:
    seq:
      - id: textid
        type: str
        size: 7
        encoding: ASCII
      - id: txtalvl
        type: u2
      - id: textdt
        type: str
        size: 14
        encoding: ASCII
      - id: txtitl
        type: str
        size: 80
        encoding: ASCII
      - id: tsclas
        type: str
        size: 1
        encoding: ASCII
      - id: tscode
        type: str
        size: 40
        encoding: ASCII
      - id: tsctlh
        type: str
        size: 40
        encoding: ASCII
      - id: tsrel
        type: str
        size: 40
        encoding: ASCII
      - id: tscatp
        type: str
        size: 1
        encoding: ASCII
      - id: tscaut
        type: str
        size: 40
        encoding: ASCII
      - id: tscrsn
        type: str
        size: 1
        encoding: ASCII
      - id: tssrdt
        type: str
        size: 8
        encoding: ASCII
      - id: tsctln
        type: str
        size: 15
        encoding: ASCII
      - id: encrypted
        type: u1
      - id: text_format
        type: str
        size: 3
        encoding: ASCII

  data_extension_segment:
    seq:
      - id: des_subheader
        type: data_extension_subheader
      - id: des_data
        size-eos: true

  data_extension_subheader:
    seq:
      - id: de
        type: str
        size: 2
        encoding: ASCII
      - id: declas
        type: str
        size: 1
        encoding: ASCII
      - id: desver
        type: u1

  reserved_extension_segment:
    seq:
      - id: res_subheader
        type: reserved_extension_subheader
      - id: res_data
        size-eos: true

  reserved_extension_subheader:
    seq:
      - id: re
        type: str
        size: 2
        encoding: ASCII
      - id: reclas
        type: str
        size: 1
        encoding: ASCII
      - id: resver
        type: u1