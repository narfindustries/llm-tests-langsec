meta:
  id: nitf
  title: National Imagery Transmission Format (NITF)
  application: 
    - Military Imagery Transmission
  xref:
    - mil_std_2500c: https://gwg.nga.mil/ntb/baseline/docs/2500c/index.html
  endian: be
doc: |
  The NITF (National Imagery Transmission Format) standard is designed to handle digital
  imagery and associated data. The format is specified by MIL-STD-2500C.

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
    repeat-expr: header.nums

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
      - id: file_security_class
        type: str
        size: 1
        encoding: ASCII
      - id: file_copy_number
        type: str
        size: 5
        encoding: ASCII
      - id: file_num_of_copys
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
        type: u4
      - id: header_length
        type: u4
      - id: numi
        type: u2
      - id: nums
        type: u2
      - id: numx
        type: u2
      - id: numt
        type: u2
      - id: numdes
        type: u2
      - id: numres
        type: u2
      - id: udidl
        type: u2
      - id: udofl
        type: u2
      - id: xhdl
        type: u2
      - id: xhdlofl
        type: u2
      - id: xudidl
        type: u2
      - id: xudidlofl
        type: u2

  image_segment:
    seq:
      - id: image_subheader
        type: image_subheader
      - id: image_data
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
      - id: issrln
        type: str
        size: 6
        encoding: ASCII
      - id: isctln
        type: str
        size: 7
        encoding: ASCII
      - id: encript
        type: u1
      - id: isorce
        type: str
        size: 42
        encoding: ASCII
      - id: nbits
        type: u1
      - id: nbands
        type: u1
      - id: xbands
        type: u2
      - id: irep
        type: str
        size: 8
        encoding: ASCII
      - id: icat
        type: str
        size: 8
        encoding: ASCII
      - id: abpp
        type: u1
      - id: pjust
        type: str
        size: 1
        encoding: ASCII
      - id: icords
        type: str
        size: 1
        encoding: ASCII
      - id: igeolo
        type: str
        size: 60
        encoding: ASCII
      - id: icom
        type: str
        size: 80
        encoding: ASCII
      - id: ic
        type: str
        size: 2
        encoding: ASCII
      - id: comrat
        type: str
        size: 4
        encoding: ASCII
      - id: nbpr
        type: u2
      - id: nbpc
        type: u2
      - id: nppbh
        type: u2
      - id: nppbv
        type: u2
      - id: nbpp
        type: u1
      - id: idlvl
        type: u2
      - id: ialvl
        type: u2
      - id: iloc
        type: str
        size: 10
        encoding: ASCII
      - id: imflt
        type: str
        size: 3
        encoding: ASCII
      - id: udidl
        type: u2
      - id: udid
        type: str
        size: udidl
        encoding: ASCII
      - id: ixshdl
        type: u2
      - id: ixshd
        type: str
        size: ixshdl
        encoding: ASCII

  graphics_segment:
    seq:
      - id: graphics_subheader
        type: graphics_subheader
      - id: graphics_data
        size-eos: true

  graphics_subheader:
    seq:
      - id: sid
        type: str
        size: 10
        encoding: ASCII
      - id: sname
        type: str
        size: 20
        encoding: ASCII
      - id: ssclas
        type: str
        size: 1
        encoding: ASCII
      - id: ssctlh
        type: str
        size: 2
        encoding: ASCII
      - id: ssrel
        type: str
        size: 20
        encoding: ASCII
      - id: ssdctp
        type: str
        size: 2
        encoding: ASCII
      - id: ssdcdt
        type: str
        size: 8
        encoding: ASCII
      - id: ssdcxm
        type: str
        size: 4
        encoding: ASCII
      - id: ssdg
        type: str
        size: 1
        encoding: ASCII
      - id: ssdgdt
        type: str
        size: 8
        encoding: ASCII
      - id: sscltx
        type: str
        size: 43
        encoding: ASCII
      - id: sscatp
        type: str
        size: 1
        encoding: ASCII
      - id: sscaut
        type: str
        size: 40
        encoding: ASCII
      - id: sscrsn
        type: str
        size: 1
        encoding: ASCII
      - id: sssrln
        type: str
        size: 6
        encoding: ASCII
      - id: ssctln
        type: str
        size: 7
        encoding: ASCII
      - id: encript
        type: u1
      - id: ssorce
        type: str
        size: 42
        encoding: ASCII
      - id: scolor
        type: str
        size: 1
        encoding: ASCII
      - id: snum
        type: u2
      - id: srot
        type: u2
      - id: slant
        type: u2
      - id: scres
        type: u4
      - id: sres2
        type: u4
      - id: sxshdl
        type: u2
      - id: sxshd
        type: str
        size: sxshdl
        encoding: ASCII

  text_segment:
    seq:
      - id: text_subheader
        type: text_subheader
      - id: text_data
        size-eos: true

  text_subheader:
    seq:
      - id: textid
        type: str
        size: 7
        encoding: ASCII
      - id: txtalvl
        type: u2
      - id: txtitl
        type: str
        size: 80
        encoding: ASCII
      - id: tsclas
        type: str
        size: 1
        encoding: ASCII
      - id: tsctlh
        type: str
        size: 2
        encoding: ASCII
      - id: tsrel
        type: str
        size: 20
        encoding: ASCII
      - id: tsdctp
        type: str
        size: 2
        encoding: ASCII
      - id: tsdcdt
        type: str
        size: 8
        encoding: ASCII
      - id: tsdcxm
        type: str
        size: 4
        encoding: ASCII
      - id: tsdg
        type: str
        size: 1
        encoding: ASCII
      - id: tsdgdt
        type: str
        size: 8
        encoding: ASCII
      - id: tscltx
        type: str
        size: 43
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
      - id: tssrln
        type: str
        size: 6
        encoding: ASCII
      - id: tsctln
        type: str
        size: 7
        encoding: ASCII
      - id: encript
        type: u1
      - id: tsfl
        type: u4
      - id: txshdl
        type: u2
      - id: txshd
        type: str
        size: txshdl
        encoding: ASCII

  data_extension_segment:
    seq:
      - id: des_subheader
        type: des_subheader
      - id: des_data
        size-eos: true

  des_subheader:
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
      - id: desclas
        type: str
        size: 1
        encoding: ASCII
      - id: desctlh
        type: str
        size: 2
        encoding: ASCII
      - id: desrel
        type: str
        size: 20
        encoding: ASCII
      - id: desdctp
        type: str
        size: 2
        encoding: ASCII
      - id: desdcdt
        type: str
        size: 8
        encoding: ASCII
      - id: desdcxm
        type: str
        size: 4
        encoding: ASCII
      - id: desdg
        type: str
        size: 1
        encoding: ASCII
      - id: desdgdt
        type: str
        size: 8
        encoding: ASCII
      - id: descltx
        type: str
        size: 43
        encoding: ASCII
      - id: descatp
        type: str
        size: 1
        encoding: ASCII
      - id: descaut
        type: str
        size: 40
        encoding: ASCII
      - id: descrsn
        type: str
        size: 1
        encoding: ASCII
      - id: dessrln
        type: str
        size: 6
        encoding: ASCII
      - id: desctln
        type: str
        size: 7
        encoding: ASCII
      - id: desofl
        type: u2
      - id: desof
        type: str
        size: desofl
        encoding: ASCII

  reserved_extension_segment:
    seq:
      - id: res_subheader
        type: res_subheader
      - id: res_data
        size-eos: true

  res_subheader:
    seq:
      - id: resid
        type: str
        size: 25
        encoding: ASCII
      - id: resver
        type: u2
      - id: resclas
        type: str
        size: 1
        encoding: ASCII
      - id: resctlh
        type: str
        size: 2
        encoding: ASCII
      - id: resrel
        type: str
        size: 20
        encoding: ASCII
      - id: resdctp
        type: str
        size: 2
        encoding: ASCII
      - id: resdcdt
        type: str
        size: 8
        encoding: ASCII
      - id: resdcxm
        type: str
        size: 4
        encoding: ASCII
      - id: resdg
        type: str
        size: 1
        encoding: ASCII
      - id: resdgdt
        type: str
        size: 8
        encoding: ASCII
      - id: rescltx
        type: str
        size: 43
        encoding: ASCII
      - id: rescatp
        type: str
        size: 1
        encoding: ASCII
      - id: rescaut
        type: str
        size: 40
        encoding: ASCII
      - id: rescrsn
        type: str
        size: 1
        encoding: ASCII
      - id: ressrln
        type: str
        size: 6
        encoding: ASCII
      - id: resctln
        type: str
        size: 7
        encoding: ASCII
      - id: resofl
        type: u2
      - id: resof
        type: str
        size: resofl
        encoding: ASCII