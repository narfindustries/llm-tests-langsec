meta:
  id: gif
  file-extension: gif
  endian: le
  title: Graphics Interference Format (GIF)
doc: |
  The Graphics Interchange Format (GIF) is a bitmap image format that was developed
  by a team at the online services provider CompuServe led by American computer scientist
  Steve Wilhite on June 15, 1987.
seq:
  - id: header
    type: header
  - id: logical_screen_descriptor
    type: logical_screen_descriptor
  - id: global_color_table
    type: color_table
    if: logical_screen_descriptor.gct_flag == 1
  - id: blocks
    type: block
    repeat: eos
types:
  header:
    seq:
      - id: magic
        size: 3
        contents: "GIF"
      - id: version
        size: 3
        contents: "89a"
  logical_screen_descriptor:
    seq:
      - id: screen_width
        type: u2
      - id: screen_height
        type: u2
      - id: flags
        type: u1
      - id: bg_color_index
        type: u1
      - id: pixel_aspect_ratio
        type: u1
    instances:
      gct_flag:
        value: flags & 0x80 != 0
      color_resolution:
        value: (flags >> 4) & 0x07
      sort_flag:
        value: (flags >> 3) & 0x01
      gct_size:
        value: 1 << ((flags & 0x07) + 1)
  color_table:
    params:
      - id: num_colors
        type: u2
    seq:
      - id: colors
        type: rgb
        repeat: expr
        repeat-expr: num_colors
  rgb:
    seq:
      - id: red
        type: u1
      - id: green
        type: u1
      - id: blue
        type: u1
  block:
    seq:
      - id: block_type
        type: u1
    instances:
      body:
        pos: _io.pos
        type:
          switch-on: block_type
          cases:
            0x2c: image_block
            0x21: extension
    types:
      image_block:
        seq:
          - id: image_left
            type: u2
          - id: image_top
            type: u2
          - id: image_width
            type: u2
          - id: image_height
            type: u2
          - id: flags
            type: u1
          - id: lzw_min_code_size
            type: u1
          - id: image_data
            type: subblocks
          - id: block_terminator
            type: u1
            contents: [0x00]
        instances:
          local_color_table:
            pos: 0
            type: color_table
            if: flags & 0x80 != 0
      extension:
        seq:
          - id: label
            type: u1
          - id: body
            type:
              switch-on: label
              cases:
                0xf9: graphic_control
                0x01: plain_text
                0xff: application
                0xfe: comment
      graphic_control:
        seq:
          - id: block_size
            type: u1
            valid:
              eq: 4
          - id: packed_fields
            type: u1
          - id: delay_time
            type: u2
          - id: transparent_color_index
            type: u1
          - id: terminator
            type: u1
            contents: [0x00]
      plain_text:
        seq:
          - id: block_size
            type: u1
          - id: text_grid_left
            type: u2
          - id: text_grid_top
            type: u2
          - id: text_grid_width
            type: u2
          - id: text_grid_height
            type: u2
          - id: char_cell_width
            type: u1
          - id: char_cell_height
            type: u1
          - id: text_fg_color_index
            type: u1
          - id: text_bg_color_index
            type: u1
          - id: plain_text_data
            type: subblocks
      application:
        seq:
          - id: block_size
            type: u1
            valid:
              eq: 11
          - id: app_identifier
            size: 8
          - id: app_auth_code
            size: 3
          - id: app_data
            type: subblocks
      comment:
        seq:
          - id: comment_data
            type: subblocks
  subblocks:
    seq:
      - id: block_size
        type: u1
      - id: data
        size: block_size
        if: block_size != 0
      - id: next_block
        type: subblocks
        if: block_size != 0