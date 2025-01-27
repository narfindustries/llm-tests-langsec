meta:
  id: jpeg
  file-extension: jpg
  endianness: le
  license: mit
seq:
  - id: soi
    type: u1
  - id: app0
    type: app0
  - id: app_segments
    type: app_segment
    repeat: until
    until: sof0
types:
  app0:
    seq:
      - id: marker
        type: u1
      - id: length
        type: u2
      - id: identifier
        type: str(0, enc: JIS_X_0201)
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
        type: str
        size: length - 2
  sof0:
    seq:
      - id: marker
        type: u1
      - id: length
        type: u2
      - id: precision
        type: u1
      - id: height
        type: u2
      - id: width
        type: u2
      - id: num_components
        type: u1
      - id: component
        type: component
        repeat: expr
        repeat-expr: num_components
  component:
    seq:
      - id: id
        type: u1
      - id: sampling_factor
        type: u1
      - id: quantization_table
        type: u1