meta:
  id: nitf
  file-extension: ntf
  endian: be
seq:
  - id: file_header
    type: file_header_type
  - id: image_segments
    type: image_segment
    repeat: expr
    repeat-expr: file_header.numi
  - id: graphic_segments
    type: graphic_segment
    repeat: expr
    repeat-expr: file_header.nums
  - id: text_segments
    type: text_segment
    repeat: expr
    repeat-expr: file_header.numt
  - id: data_extension_segments
    type: data_extension_segment
    repeat: expr
    repeat-expr: file_header.numdes
  - id: reserved_extension_segments
    type: reserved_extension_segment
    repeat: expr
    repeat-expr: file_header.numres
types:
  file_header_type:
    seq:
      - id: fhdr
        type: str
        size: 4
        encoding: ascii
      - id: fver
        type: str
        size: 5
        encoding: ascii
      - id: clevel
        type: str
        size: 2
        encoding: ascii
      - id: stype
        type: str
        size: 4
        encoding: ascii
      - id: ostaid
        type: str
        size: 10
        encoding: ascii
      - id: fdt
        type: str
        size: 14
        encoding: ascii
      - id: ftitle
        type: str
        size: 80
        encoding: ascii
      - id: fsclas
        type: str
        size: 1
        encoding: ascii
      - id: fsclsy
        type: str
        size: 2
        encoding: ascii
      - id: fscode
        type: str
        size: 11
        encoding: ascii
      - id: fsctlh
        type: str
        size: 2
        encoding: ascii
      - id: fsrel
        type: str
        size: 20
        encoding: ascii
      - id: fsdctp
        type: str
        size: 2
        encoding: ascii
      - id: fsdcdt
        type: str
        size: 8
        encoding: ascii
      - id: fsdcxm
        type: str
        size: 4
        encoding: ascii
      - id: fsdg
        type: str
        size: 1
        encoding: ascii
      - id: fsdgdt
        type: str
        size: 8
        encoding: ascii
      - id: fscltx
        type: str
        size: 43
        encoding: ascii
      - id: fscatp
        type: str
        size: 1
        encoding: ascii
      - id: fscaut
        type: str
        size: 40
        encoding: ascii
      - id: fscrsn
        type: str
        size: 1
        encoding: ascii
      - id: fssrdt
        type: str
        size: 8
        encoding: ascii
      - id: fsctln
        type: str
        size: 15
        encoding: ascii
      - id: fscop
        type: str
        size: 5
        encoding: ascii
      - id: fscpys
        type: str
        size: 5
        encoding: ascii
      - id: encryp
        type: str
        size: 1
        encoding: ascii
      - id: fbkgc
        type: str
        size: 3
        encoding: ascii
      - id: oname
        type: str
        size: 24
        encoding: ascii
      - id: ophone
        type: str
        size: 18
        encoding: ascii
      - id: fl
        type: str
        size: 12
        encoding: ascii
      - id: hl
        type: str
        size: 6
        encoding: ascii
      - id: numi
        type: u4
      - id: lish
        type: str
        size: 6
        encoding: ascii
        repeat: expr
        repeat-expr: numi
      - id: li
        type: str
        size: 10
        encoding: ascii
        repeat: expr
        repeat-expr: numi
      - id: nums
        type: u4
      - id: lssh
        type: str
        size: 4
        encoding: ascii
        repeat: expr
        repeat-expr: nums
      - id: ls
        type: str
        size: 6
        encoding: ascii
        repeat: expr
        repeat-expr: nums
      - id: numt
        type: u4
      - id: ltsh
        type: str
        size: 4
        encoding: ascii
        repeat: expr
        repeat-expr: numt
      - id: lt
        type: str
        size: 5
        encoding: ascii
        repeat: expr
        repeat-expr: numt
      - id: numdes
        type: u4
      - id: ldsh
        type: str
        size: 4
        encoding: ascii
        repeat: expr
        repeat-expr: numdes
      - id: ld
        type: str
        size: 9
        encoding: ascii
        repeat: expr
        repeat-expr: numdes
      - id: numres
        type: u4
      - id: lresh
        type: str
        size: 4
        encoding: ascii
        repeat: expr
        repeat-expr: numres
      - id: lre
        type: str
        size: 7
        encoding: ascii
        repeat: expr
        repeat-expr: numres
      - id: udhdl
        type: str
        size: 5
        encoding: ascii
      - id: udhofl
        type: str
        size: 3
        encoding: ascii
        if: udhdl.to_i > 0
      - id: udhd
        size: udhdl.to_i - 3
        if: udhdl.to_i > 0
      - id: xhdl
        type: str
        size: 5
        encoding: ascii
      - id: xhdlofl
        type: str
        size: 3
        encoding: ascii
        if: xhdl.to_i > 0
      - id: xhd
        size: xhdl.to_i - 3
        if: xhdl.to_i > 0
  image_segment:
    seq:
      - id: im
        type: image_subheader
      - id: image_data
        size: im.image_length.to_i
  image_subheader:
    seq:
      - id: im
        type: str
        size: 2
        encoding: ascii
      - id: iid1
        type: str
        size: 10
        encoding: ascii
      - id: idatim
        type: str
        size: 14
        encoding: ascii
      - id: tgtid
        type: str
        size: 17
        encoding: ascii
      - id: iid2
        type: str
        size: 80
        encoding: ascii
      - id: isclas
        type: str
        size: 1
        encoding: ascii
      - id: isclsy
        type: str
        size: 2
        encoding: ascii
      - id: iscode
        type: str
        size: 11
        encoding: ascii
      - id: isctlh
        type: str
        size: 2
        encoding: ascii
      - id: isrel
        type: str
        size: 20
        encoding: ascii
      - id: isdctp
        type: str
        size: 2
        encoding: ascii
      - id: isdcdt
        type: str
        size: 8
        encoding: ascii
      - id: isdcxm
        type: str
        size: 4
        encoding: ascii
      - id: isdg
        type: str
        size: 1
        encoding: ascii
      - id: isdgdt
        type: str
        size: 8
        encoding: ascii
      - id: iscltx
        type: str
        size: 43
        encoding: ascii
      - id: iscatp
        type: str
        size: 1
        encoding: ascii
      - id: iscaut
        type: str
        size: 40
        encoding: ascii
      - id: iscrsn
        type: str
        size: 1
        encoding: ascii
      - id: issrdt
        type: str
        size: 8
        encoding: ascii
      - id: isctln
        type: str
        size: 15
        encoding: ascii
      - id: encryp
        type: str
        size: 1
        encoding: ascii
      - id: isorce
        type: str
        size: 42
        encoding: ascii
      - id: nrows
        type: str
        size: 8
        encoding: ascii
      - id: ncols
        type: str
        size: 8
        encoding: ascii
      - id: pvtype
        type: str
        size: 3
        encoding: ascii
      - id: irep
        type: str
        size: 8
        encoding: ascii
      - id: icat
        type: str
        size: 8
        encoding: ascii
      - id: abpp
        type: str
        size: 2
        encoding: ascii
      - id: pjust
        type: str
        size: 1
        encoding: ascii
      - id: icords
        type: str
        size: 1
        encoding: ascii
      - id: igeolo
        type: str
        size: 60
        encoding: ascii
        if: icords != ' '
      - id: nicom
        type: str
        size: 1
        encoding: ascii
      - id: icom
        type: str
        size: 80
        encoding: ascii
        repeat: expr
        repeat-expr: nicom.to_i
      - id: ic
        type: str
        size: 2
        encoding: ascii
      - id: comrat
        type: str
        size: 4
        encoding: ascii
        if: ic != 'NC'
      - id: nbands
        type: str
        size: 1
        encoding: ascii
      - id: xbands
        type: str
        size: 5
        encoding: ascii
        if: nbands == '0'
      - id: irepband
        type: str
        size: 2
        encoding: ascii
        repeat: expr
        repeat-expr: nbands.to_i
      - id: isubcat
        type: str
        size: 6
        encoding: ascii
        repeat: expr
        repeat-expr: nbands.to_i
      - id: ifc
        type: str
        size: 1
        encoding: ascii
        repeat: expr
        repeat-expr: nbands.to_i
      - id: imflt
        type: str
        size: 3
        encoding: ascii
        repeat: expr
        repeat-expr: nbands.to_i
      - id: nluts
        type: str
        size: 1
        encoding: ascii
        repeat: expr
        repeat-expr: nbands.to_i
      - id: nelut
        type: str
        size: 5
        encoding: ascii
        repeat: expr
        repeat-expr: nbands.to_i
      - id: lutd
        size: 1
        repeat: expr
        repeat-expr: nbands.to_i * nelut[_index].to_i
        if: nluts[0].to_i > 0
      - id: isync
        type: str
        size: 1
        encoding: ascii
      - id: imode
        type: str
        size: 1
        encoding: ascii
      - id: nbpr
        type: str
        size: 4
        encoding: ascii
      - id: nbpc
        type: str
        size: 4
        encoding: ascii
      - id: nppbh
        type: str
        size: 4
        encoding: ascii
      - id: nppbv
        type: str
        size: 4
        encoding: ascii
      - id: nbpp
        type: str
        size: 2
        encoding: ascii
      - id: idlvl
        type: str
        size: 3
        encoding: ascii
      - id: ialvl
        type: str
        size: 3
        encoding: ascii
      - id: iloc
        type: str
        size: 10
        encoding: ascii
      - id: imag
        type: str
        size: 4
        encoding: ascii
      - id: udidl
        type: str
        size: 5
        encoding: ascii
      - id: udofl
        type: str
        size: 3
        encoding: ascii
        if: udidl.to_i > 0
      - id: udid
        size: udidl.to_i - 3
        if: udidl.to_i > 0
      - id: ixshdl
        type: str
        size: 5
        encoding: ascii
      - id: ixsofl
        type: str
        size: 3
        encoding: ascii
        if: ixshdl.to_i > 0
      - id: ixshd
        size: ixshdl.to_i - 3
        if: ixshdl.to_i > 0
      - id: image_length
        type: str
        size: 10
        encoding: ascii
  graphic_segment:
    seq:
      - id: graphic_subheader
        size: _parent.file_header.lssh[_index].to_i
      - id: graphic_data
        size: _parent.file_header.ls[_index].to_i
  text_segment:
    seq:
      - id: text_subheader
        size: _parent.file_header.ltsh[_index].to_i
      - id: text_data
        size: _parent.file_header.lt[_index].to_i
  data_extension_segment:
    seq:
      - id: des_subheader
        size: _parent.file_header.ldsh[_index].to_i
      - id: des_data
        size: _parent.file_header.ld[_index].to_i
  reserved_extension_segment:
    seq:
      - id: res_subheader
        size: _parent.file_header.lresh[_index].to_i
      - id: res_data
        size: _parent.file_header.lre[_index].to_i