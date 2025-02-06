meta:
  id: nitf
  title: National Imagery Transmission Format (NITF)
  file-extension: nitf
  endian: be
  encoding: utf-8

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
  - id: extension_segments
    type: extension_segment
    repeat: expr
    repeat-expr: file_header.num_extension_segments

types:
  file_header:
    seq:
      - id: fhdr
        type: str
        size: 4
      - id: clevel
        type: str
        size: 2
      - id: stype
        type: str
        size: 4
      - id: ostaid
        type: str
        size: 10
      - id: ftitle
        type: str
        size: 80
      - id: fsclas
        type: str
        size: 1
      - id: fscode
        type: str
        size: 2
        if: fsclas != "U"
      - id: fsctlh
        type: str
        size: 20
        if: fsclas != "U"
      - id: fsrel
        type: str
        size: 20
        if: fsclas != "U"
      - id: encryp
        type: str
        size: 1
      - id: ftxtd
        type: u4
      - id: num_image_segments
        type: u2
      - id: num_graphic_segments
        type: u2
      - id: num_text_segments
        type: u2
      - id: num_extension_segments
        type: u2

  image_segment:
    seq:
      - id: isubhdr
        type: image_subheader
      - id: image_data
        size: isubhdr.data_length

  image_subheader:
    seq:
      - id: iid1
        type: str
        size: 10
      - id: idatim
        type: str
        size: 14
      - id: ititle
        type: str
        size: 80
      - id: isclas
        type: str
        size: 1
      - id: iscocode
        type: str
        size: 2
        if: isclas != "U"
      - id: isctlh
        type: str
        size: 20
        if: isclas != "U"
      - id: isrel
        type: str
        size: 20
        if: isclas != "U"
      - id: nbands
        type: u1
      - id: icords
        type: str
        size: 1
      - id: data_length
        type: u4

  graphic_segment:
    seq:
      - id: gsubhdr
        type: graphic_subheader
      - id: graphic_data
        size: gsubhdr.data_length

  graphic_subheader:
    seq:
      - id: gid
        type: str
        size: 10
      - id: gdatim
        type: str
        size: 14
      - id: gtitle
        type: str
        size: 80
      - id: gsclas
        type: str
        size: 1
      - id: gscocode
        type: str
        size: 2
        if: gsclas != "U"
      - id: gsctlh
        type: str
        size: 20
        if: gsclas != "U"
      - id: gsrel
        type: str
        size: 20
        if: gsclas != "U"
      - id: data_length
        type: u4

  text_segment:
    seq:
      - id: tsubhdr
        type: text_subheader
      - id: text_data
        size: tsubhdr.data_length

  text_subheader:
    seq:
      - id: tid
        type: str
        size: 10
      - id: tdatim
        type: str
        size: 14
      - id: ttitle
        type: str
        size: 80
      - id: tsclas
        type: str
        size: 1
      - id: tscocode
        type: str
        size: 2
        if: tsclas != "U"
      - id: tsctlh
        type: str
        size: 20
        if: tsclas != "U"
      - id: tsrel
        type: str
        size: 20
        if: tsclas != "U"
      - id: data_length
        type: u4

  extension_segment:
    seq:
      - id: esubhdr
        type: extension_subheader
      - id: extension_data
        size: esubhdr.data_length

  extension_subheader:
    seq:
      - id: eid
        type: str
        size: 10
      - id: edatim
        type: str
        size: 14
      - id: etitle
        type: str
        size: 80
      - id: esclas
        type: str
        size: 1
      - id: escocode
        type: str
        size: 2
        if: esclas != "U"
      - id: esctlh
        type: str
        size: 20
        if: esclas != "U"
      - id: esrel
        type: str
        size: 20
        if: esclas != "U"
      - id: data_length
        type: u4