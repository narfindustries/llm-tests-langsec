meta:
  id: nitf
  file-extension: nitf
  endian: be
seq:
  - id: file_header
    type: file_header
  - id: image_segments
    type: image_segment
    repeat: expr
    repeat-expr: file_header.onum
  - id: graphic_segments
    type: graphic_segment
    repeat: expr
    repeat-expr: file_header.tnum
  - id: text_segments
    type: text_segment
    repeat: expr
    repeat-expr: file_header.tnum
  - id: data_extension_segments
    type: data_extension_segment
    repeat: expr
    repeat-expr: file_header.enum
  - id: reserved_extension_segments
    type: reserved_extension_segment
    repeat: expr
    repeat-expr: file_header.rnum
  - id: end_of_file
    type: end_of_file
types:
  file_header:
    seq:
      - id: fileid
        type: str
        size: 9
        encoding: UTF-8
      - id: fhdr
        type: u2
      - id: clevel
        type: u1
      - id: stype
        type: str
        size: 4
        encoding: UTF-8
      - id: ostaid
        type: str
        size: 10
        encoding: UTF-8
      - id: dt
        type: str
        size: 14
        encoding: UTF-8
      - id: ftitle
        type: str
        size: 80
        encoding: UTF-8
      - id: onum
        type: u2
      - id: tnum
        type: u2
      - id: tsize
        type: u4
      - id: enum
        type: u2
      - id: rnum
        type: u2
  image_segment:
    seq:
      - id: iid
        type: str
        size: 10
        encoding: UTF-8
      - id: idatim
        type: str
        size: 14
        encoding: UTF-8
      - id: ititle
        type: str
        size: 80
        encoding: UTF-8
      - id: isorce
        type: str
        size: 42
        encoding: UTF-8
      - id: nrows
        type: u4
      - id: ncols
        type: u4
      - id: pvtype
        type: str
        size: 3
        encoding: UTF-8
      - id: irep
        type: str
        size: 8
        encoding: UTF-8
      - id: icat
        type: str
        size: 8
        encoding: UTF-8
  graphic_segment:
    seq:
      - id: sid
        type: str
        size: 10
        encoding: UTF-8
      - id: sname
        type: str
        size: 20
        encoding: UTF-8
      - id: scolor
        type: str
        size: 1
        encoding: UTF-8
      - id: sxorig
        type: u4
      - id: syorig
        type: u4
  text_segment:
    seq:
      - id: textid
        type: str
        size: 7
        encoding: UTF-8
      - id: txtalvl
        type: u1
      - id: txtdt
        type: str
        size: 14
        encoding: UTF-8
      - id: txtitl
        type: str
        size: 80
        encoding: UTF-8
  data_extension_segment:
    seq:
      - id: desid
        type: str
        size: 25
        encoding: UTF-8
      - id: desver
        type: u1
      - id: desoflw
        type: u1
      - id: desitem
        type: str
        size: 3
        encoding: UTF-8
  reserved_extension_segment:
    seq:
      - id: reid
        type: str
        size: 25
        encoding: UTF-8
      - id: rever
        type: u1
      - id: reoflw
        type: u1
      - id: reitem
        type: str
        size: 3
        encoding: UTF-8
  end_of_file:
    seq:
      - id: eof
        type: str
        size: 3
        encoding: UTF-8