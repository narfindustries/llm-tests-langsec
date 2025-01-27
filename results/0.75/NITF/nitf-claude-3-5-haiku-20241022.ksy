meta:
  id: nitf
  file-extension: nitf
  endian: be
seq:
  - id: header
    type: file_header
  - id: image_segments
    type: image_segment
    repeat: eos
types:
  file_header:
    seq:
      - id: fhdr
        contents: "NITF"
      - id: fver
        size: 2
      - id: clevel
        size: 2
      - id: stype
        size: 4
      - id: ostaid
        size: 10
      - id: fdt
        size: 14
      - id: ftitle
        size: 80
      - id: fpseudo
        size: 1
      - id: num_image_segments
        type: u2
  image_segment:
    seq:
      - id: header
        type: image_subheader
      - id: image_data
        size-eos: true
  image_subheader:
    seq:
      - id: im
        contents: "IM"
      - id: iid1
        size: 10
      - id: idatim
        size: 14
      - id: tgt
        size: 17
      - id: iid2
        size: 80
      - id: isclas
        size: 1