meta:
  id: jpeg
  file-extension: jpg
  endian: be
seq:
  - id: soi
    type: u1
  - id: app0
    type: app0
  - id: app_segments
    type: app_segment
    repeat: until
    repeat-until: marker == 0xd9
types:
  app0:
    seq:
      - id: marker
        type: u1
      - id: length
        type: u2
      - id: identifier
        type: str(0x10, enc: JIS_X_0201)
      - id: version
        type: u1
      - id: units
        type: u1
      - id: x_density
        type: u2
      - id: y_density
        type: u2
      - id: thumb_width
        type: u1
      - id: thumb_height
        type: u1
  app_segment:
    seq:
      - id: marker
        type: u1
      - id: length
        type: u2
      - id: data
        type: bytes
        size: length - 2