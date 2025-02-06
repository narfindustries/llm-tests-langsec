meta:
  id: nitf
  title: National Imagery Transmission Format (NITF)
  file-extension: nitf
  endian: be
  xref:
    wikidata: Q6953607

doc: |
  The NITF (National Imagery Transmission Format) standard is designed to
  handle formatted image and associated textual and other data. NITF is
  used by the US government and military for transmitting digital images.

seq:
  - id: header
    type: file_header

  - id: image_segments
    type: image_segment
    repeat: expr
    repeat-expr: header.numi

  - id: graphics_segments
    type: graphics_segment
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
      - id: clevel
        type: str
        size: 2
        encoding: ASCII
      - id: stype
        type: str
        size: 4
        encoding: ASCII
      - id: osta_id
        type: str
        size: 10
        encoding: ASCII
      - id: fdt
        type: str
        size: 14
        encoding: ASCII
      - id: ftitle
        type: str
        size: 80
        encoding: ASCII
      - id: fsclas
        type: str
        size: 1
        encoding: ASCII
      - id: fsclas_sys
        type: str
        size: 2
        encoding: ASCII
      - id: fscode
        type: str
        size: 11
        encoding: ASCII
      - id: fsctlh
        type: str
        size: 2
        encoding: ASCII
      - id: fsrel
        type: str
        size: 20
        encoding: ASCII
      - id: fsdctp
        type: str
        size: 2
        encoding: ASCII
      - id: fsdcdt
        type: str
        size: 8
        encoding: ASCII
      - id: fsdcxm
        type: str
        size: 4
        encoding: ASCII
      - id: fsdg
        type: str
        size: 1
        encoding: ASCII
      - id: fsdgdt
        type: str
        size: 8
        encoding: ASCII
      - id: fscltx
        type: str
        size: 43
        encoding: ASCII
      - id: fscatp
        type: str
        size: 1
        encoding: ASCII
      - id: fscaut
        type: str
        size: 40
        encoding: ASCII
      - id: fscrsn
        type: str
        size: 1
        encoding: ASCII
      - id: fssrdt
        type: str
        size: 8
        encoding: ASCII
      - id: fsctln
        type: str
        size: 15
        encoding: ASCII
      - id: fscop
        type: str
        size: 5
        encoding: ASCII
      - id: fscpys
        type: str
        size: 5
        encoding: ASCII
      - id: encryp
        type: str
        size: 1
        encoding: ASCII
      - id: fbkgc
        type: str
        size: 3
        encoding: ASCII
      - id: oname
        type: str
        size: 27
        encoding: ASCII
      - id: ophone
        type: str
        size: 18
        encoding: ASCII
      - id: numi
        type: u2
      - id: linfo
        type: length_info
        repeat: expr
        repeat-expr: numi
      - id: numg
        type: u1
      - id: numx
        type: u1
      - id: numt
        type: u1
      - id: numdes
        type: u1
      - id: numres
        type: u1
      - id: udidl
        type: u4
      - id: udid
        size: udidl
      - id: xhdl
        type: u4
      - id: xhd
        size: xhdl

  length_info:
    seq:
      - id: length_image_segment
        type: u4

  image_segment:
    seq:
      - id: image_subheader
        type: image_subheader
      - id: image_data_field
        size-eos: true

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
      - id: isclas_sys
        type: str
        size: 2
        encoding: ASCII
      - id: iscode
        type: str
        size: 11
        encoding: ASCII
      - id: isctlh
        type: str
        size: 2
        encoding: ASCII
      - id: isrel
        type: str
        size: 20
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
      - id: isdgdt
        type: str
        size: 8
        encoding: ASCII
      - id: iscltx
        type: str
        size: 43
        encoding: ASCII
      - id: iscatp
        type: str
        size: 1
        encoding: ASCII
      - id: iscaut
        type: str
        size: 40
        encoding: ASCII
      - id: iscrsn
        type: str
        size: 1
        encoding: ASCII
      - id: issrdt
        type: str
        size: 8
        encoding: ASCII
      - id: isctln
        type: str
        size: 15
        encoding: ASCII
      - id: isorce
        type: str
        size: 42
        encoding: ASCII

  graphics_segment:
    seq:
      - id: graphic_subheader
        type: graphic_subheader
      - id: graphic_data_field
        size-eos: true

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

  text_segment:
    seq:
      - id: text_subheader
        type: text_subheader
      - id: text_data_field
        size-eos: true

  text_subheader:
    seq:
      - id: textid
        type: str
        size: 7
        encoding: ASCII
      - id: txtalvl
        type: u2
      - id: txtdate
        type: str
        size: 14
        encoding: ASCII
      - id: txttitl
        type: str
        size: 80
        encoding: ASCII
      - id: txtcls
        type: str
        size: 1
        encoding: ASCII

  data_extension_segment:
    seq:
      - id: des_subheader
        type: des_subheader
      - id: des_data_field
        size-eos: true

  des_subheader:
    seq:
      - id: desid
        type: str
        size: 25
        encoding: ASCII
      - id: desver
        type: u1
      - id: dessclas
        type: str
        size: 1
        encoding: ASCII

  reserved_extension_segment:
    seq:
      - id: res_subheader
        type: res_subheader
      - id: res_data_field
        size-eos: true

  res_subheader:
    seq:
      - id: res_type
        type: str
        size: 25
        encoding: ASCII
      - id: res_version
        type: u1
      - id: ressclas
        type: str
        size: 1
        encoding: ASCII