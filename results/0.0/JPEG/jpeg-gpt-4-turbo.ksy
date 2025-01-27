meta:
  id: jpeg
  file-extension: jpg
  endian: le
  title: JPEG (JFIF format)
  license: CC0-1.0
  ks-version: 0.9

doc: |
  JPEG File Interchange Format, or JFIF, is a graphical data format that enables the compression and storage of digital images using the JPEG standard.

seq:
  - id: segments
    type: segment
    repeat: eos

types:
  segment:
    seq:
      - id: marker_prefix
        contents: [0xFF]
      - id: marker
        type: u1
        enum: jpeg_marker
      - id: length
        type: u2
        if: marker != jpeg_marker::soi and marker != jpeg_marker::eoi
      - id: data
        size: length - 2
        if: marker != jpeg_marker::soi and marker != jpeg_marker::eoi

enums:
  jpeg_marker:
    0xd8: soi
    0xc0: sof0
    0xc4: dht
    0xdb: dqt
    0xdd: dri
    0xda: sos
    0xd9: eoi