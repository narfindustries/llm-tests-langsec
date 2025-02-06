meta:
  id: nitf
  file-extension: ntf
  endian: be
  encoding: ASCII
seq:
  - id: file_header
    type: file_header
  - id: image_segments
    type: image_segment
    repeat: eos

types:
  file_header:
    seq:
      - id: fhdr
        type: str
        size: 9
      - id: clevel
        type: str
        size: 2
      - id: stype
        type: str
        size: 1
      - id: orig_station_id
        type: str
        size: 10
      - id: fdt
        type: str
        size: 14
      - id: ftitle
        type: str
        size: 80
      - id: fscop
        type: str
        size: 1
      - id: fscpys
        type: str
        size: 5
      - id: encryp
        type: str
        size: 1
      - id: filesfb
        type: str
        size: 3
      - id: fl
        type: str
        size: 12
      - id: hl
        type: str
        size: 6
      - id: num_image_segments
        type: str
        size: 3
      - id: num_graphic_segments
        type: str
        size: 3
      - id: num_text_segments
        type: str
        size: 3
      - id: num_des_segments
        type: str
        size: 3
      - id: num_reserved_segments
        type: str
        size: 3
      - id: user_defined_header
        type: user_defined_header
      - id: extended_header
        type: extended_header

  user_defined_header:
    seq:
      - id: length
        type: str
        size: 5
      - id: data
        type: str
        size-eos: true

  extended_header:
    seq:
      - id: length
        type: str
        size: 5
      - id: data
        type: str
        size-eos: true

  image_segment:
    seq:
      - id: image_subheader
        type: image_subheader
      - id: image_data
        type: image_data

  image_subheader:
    seq:
      - id: im
        type: str
        size: 10
      - id: isorce
        type: str
        size: 42
      - id: nrows
        type: str
        size: 8
      - id: ncols
        type: str
        size: 8
      - id: pvtype
        type: str
        size: 3
      - id: irep
        type: str
        size: 8
      - id: icat
        type: str
        size: 8
      - id: abpp
        type: str
        size: 2
      - id: pjust
        type: str
        size: 1
      - id: imode
        type: str
        size: 1
      - id: nbands
        type: str
        size: 1
      - id: xbands
        type: str
        size: 5
        if: nbands == '0'
      - id: band_info
        type: band_info
        repeat: expr
        repeat-expr: '(nbands == ''0'') ? (int(xbands)) : (int(nbands))'
      - id: isync
        type: str
        size: 1
      - id: security_group
        type: security_group

  band_info:
    seq:
      - id: irepband
        type: str
        size: 2
      - id: isubcat
        type: str
        size: 6
      - id: ifc
        type: str
        size: 1
      - id: imflt
        type: str
        size: 3
      - id: nluts
        type: str
        size: 1
      - id: lut_data
        type: lut_data
        repeat: expr
        repeat-expr: 'int(nluts)'

  lut_data:
    seq:
      - id: entry_count
        type: str
        size: 5
      - id: lut_entries
        type: str
        size: 'int(entry_count)'

  security_group:
    seq:
      - id: fsdwng
        type: str
        size: 2
      - id: fsdevt
        type: str
        size: 10
      - id: fsdgdt
        type: str
        size: 8
      - id: fsdgdt2
        type: str
        size: 4
      - id: fssclas
        type: str
        size: 1
      - id: fsscop
        type: str
        size: 2
      - id: fsscpys
        type: str
        size: 5

  image_data:
    seq:
      - id: data
        type: str
        size-eos: true