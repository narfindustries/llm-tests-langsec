meta:
  id: jpeg
  file-extension: jpg
  endian: le
  title: "JPEG image format"
  license: CC0-1.0
seq:
  - id: segments
    type: segment
    repeat: eos
types:
  segment:
    seq:
      - id: marker
        type: u2
        enum: markers
      - id: length
        type: u2
        if: marker != markers::soi and marker != markers::eoi
      - id: data
        size: length - 2
        if: length > 2
enums:
  markers:
    0xffd8: soi
    0xffd9: eoi
    0xffe0: app0
    0xffdb: dqt
    0xffc0: sof0
    0xffc4: dht
    0xffda: sos