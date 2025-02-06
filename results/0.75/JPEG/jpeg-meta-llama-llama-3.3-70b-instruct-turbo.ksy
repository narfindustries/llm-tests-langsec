meta:
  id: jpeg
  endian: be

seq:
  - id: soi
    type: u2
    if: $data == 0xffd8

  - id: app0
    type:
      seq:
        - id: marker
          type: u2
        - id: length
          type: u2
        - id: identifier
          type: str
          size: 5
          encoding: ascii
        - id: version
          type: u2
        - id: units
          type: u1
        - id: xdensity
          type: u2
        - id: ydensity
          type: u2
        - id: thumbnail_width
          type: u1
        - id: thumbnail_height
          type: u1
    if: soi.value == 0xffd8 and app0.marker == 0xffe0

  - id: app1
    type:
      seq:
        - id: marker
          type: u2
        - id: length
          type: u2
        - id: identifier
          type: str
          size: len_data
          encoding: ascii
          if: _parent.marker == 0xffe1
        - id: exif_data
          type: exif
          if: _parent.marker == 0xffe1 and _parent.identifier == "Exif"
        - id: ICC_profile
          type: ICC_profile
          if: _parent.marker == 0xffe1 and _parent.identifier != "Exif"
    if: soi.value == 0xffd8 and (app0.marker == 0xffe0 or app1.marker == 0xffe1)

  - id: dqt
    type:
      seq:
        - id: marker
          type: u2
        - id: length
          type: u2
        - id: table_number
          type: u1
        - id: precision
          type: u1
        - id: table_data
          type: u1
          count: 64
    if: soi.value == 0xffd8 and (app0.marker == 0xffe0 or app1.marker == 0xffe1 or dqt.marker == 0xffdb)

  - id: dht
    type:
      seq:
        - id: marker
          type: u2
        - id: length
          type: u2
        - id: table_class
          type: u1
        - id: table_destination
          type: u1
        - id: number_of_codes
          type: u1
        - id: code_lengths
          type: u1
          count: 16
        - id: code_values
          type: u1
          count: 256
    if: soi.value == 0xffd8 and (app0.marker == 0xffe0 or app1.marker == 0xffe1 or dqt.marker == 0xffdb or dht.marker == 0xffc4)

  - id: sof0
    type:
      seq:
        - id: marker
          type: u2
        - id: length
          type: u2
        - id: precision
          type: u1
        - id: height
          type: u2
        - id: width
          type: u2
        - id: number_of_components
          type: u1
        - id: components
          type:
            seq:
              - id: id
                type: u1
              - id: horizontal_sampling_factor
                type: u1
              - id: vertical_sampling_factor
                type: u1
              - id: quantization_table_number
                type: u1
            repeat: expr
    if: soi.value == 0xffd8 and (app0.marker == 0xffe0 or app1.marker == 0xffe1 or dqt.marker == 0xffdb or dht.marker == 0xffc4 or sof0.marker == 0xffc0)

  - id: sos
    type:
      seq:
        - id: marker
          type: u2
        - id: length
          type: u2
        - id: number_of_components
          type: u1
        - id: components
          type:
            seq:
              - id: id
                type: u1
              - id: dc_coefficient_prediction
                type: u1
              - id: ac_coefficient_prediction
                type: u1
            repeat: expr
    if: soi.value == 0xffd8 and (app0.marker == 0xffe0 or app1.marker == 0xffe1 or dqt.marker == 0xffdb or dht.marker == 0xffc4 or sof0.marker == 0xffc0 or sos.marker == 0xffda)

  - id: eoi
    type: u2
    if: $data == 0xffd9

types:
  exif:
    seq:
      - id: exif_header
        type: str
        size: 2
        encoding: ascii
      - id: tiff_header
        type: u2
        if: $data == 0x4949
      - id: offset
        type: u4

  ICC_profile:
    seq:
      - id: profile_header
        type: str
        size: 4
        encoding: ascii
      - id: profile_data
        type: u1
        size: len_data

  expr:
    id: number_of_components
    type: u1