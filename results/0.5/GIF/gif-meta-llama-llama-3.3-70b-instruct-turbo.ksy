meta:
  id: gif
  endian: le
seq:
  - id: signature
    type: str
    size: 3
    enum: ['GIF']
  - id: version
    type: str
    size: 3
    enum: ['87a', '89a']
  - id: logical_screen_width
    type: u2
  - id: logical_screen_height
    type: u2
  - id: packed_fields
    type: u1
  - id: background_color_index
    type: u1
  - id: pixel_aspect_ratio
    type: u1
  - id: global_color_table
    type: seq
    repeat: expr
    seq:
      - id: red
        type: u1
      - id: green
        type: u1
      - id: blue
        type: u1
    repeat-expr: 2 ** ((packed_fields & 7) + 1)
  - id: images
    type: seq
    repeat: until
    until: expr
    seq:
      - id: image_separator
        type: u1
        enum: [44]
      - id: image_left
        type: u2
      - id: image_top
        type: u2
      - id: image_width
        type: u2
      - id: image_height
        type: u2
      - id: packed_fields_2
        type: u1
      - id: local_color_table
        type: seq
        if: expr
        seq:
          - id: red
            type: u1
          - id: green
            type: u1
          - id: blue
            type: u1
        repeat: expr
        repeat-expr: 2 ** ((packed_fields_2 & 7) + 1)
        if-expr: packed_fields_2 & 128 != 0
      - id: image_data
        type: lzw
      - id: extensions
        type: seq
        repeat: until
        until: expr
        seq:
          - id: extension_introducer
            type: u1
            enum: [33]
          - id: extension_label
            type: u1
          - id: extension_data
            type: 
              switch-on: extension_label
              cases:
                254: 
                  type: seq
                  seq:
                    - id: block_size
                      type: u1
                    - id: data
                      type: str
                      size: block_size
                255: 
                  type: seq
                  seq:
                    - id: block_size
                      type: u1
                    - id: data
                      type: str
                      size: block_size
                249: 
                  type: seq
                  seq:
                    - id: block_size
                      type: u1
                    - id: data
                      type: str
                      size: block_size
          until-expr: _io.eof() or extension_label == 0
    until-expr: _io.eof()
  - id: trailer
    type: u1
    enum: [59]