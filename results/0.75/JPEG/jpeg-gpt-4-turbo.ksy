meta:
  id: jpeg
  file-extension: jpg
  endian: le
  title: "JPEG image file"
  license: CC0-1.0
  ks-version: 0.9

seq:
  - id: segments
    type: segment
    repeat: eos

types:
  segment:
    seq:
      - id: marker
        type: u2
      - id: length
        type: u2
        if: marker != 0xffd9
      - id: data
        size: length - 2
        if: marker != 0xffd9

enums:
  marker_enum:
    0xffd8: soi
    0xffc0: sof0
    0xffc2: sof2
    0xffdb: dqt
    0xffdd: dri
    0xffda: sos
    0xffd9: eoi