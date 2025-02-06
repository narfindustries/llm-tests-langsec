meta:
  id: gif
  endian: be
seq:
  - id: signature
    type: str
    len: 6
    encoding: ASCII
  - id: logical_screen_descriptor
    seq:
      - id: width
        type: u2
      - id: height
        type: u2
      - id: packed_fields
        type: u1
        fields:
          - id: global_color_table_flag
            type: bool
            bits: 1
          - id: color_resolution
            type: u3
            bits: 3
          - id: sort_flag
            type: bool
            bits: 1
          - id: global_color_table_size
            type: u3
            bits: 3
      - id: global_color_table
        type: color_table
        if: this.logical_screen_descriptor.packed_fields.global_color_table_flag
        repeat: expr
          - size: 1 << (this.logical_screen_descriptor.packed_fields.global_color_table_size + 1)
  - id: blocks
    type: block
    repeat: true
    until: this.blocks[-1].type == 0x3B

types:
  block:
    seq:
      - id: type
        type: u1
      - id: body
        switch: type
          - 0x2C:
              seq:
                - id: image_left_position
                  type: u2
                - id: image_top_position
                  type: u2
                - id: image_width
                  type: u2
                - id: image_height
                  type: u2
                - id: packed_fields
                  type: u1
                  fields:
                    - id: local_color_table_flag
                      type: bool
                      bits: 1
                    - id: interlace_flag
                      type: bool
                      bits: 1
                    - id: sort_flag
                      type: bool
                      bits: 1
                    - id: reserved
                      type: u2
                      bits: 2
                    - id: local_color_table_size
                      type: u3
                      bits: 3
                - id: local_color_table
                  type: color_table
                  if: this.packed_fields.local_color_table_flag
                  repeat: expr
                    - size: 1 << (this.packed_fields.local_color_table_size + 1)
                - id: lzw_data
                  type: lzw_data
          - 0x21:
              seq:
                - id: extension_type
                  type: u1
                - id: extension_data
                  type: extension_data
          - 0x3B:
              pass
  color_table:
    seq:
      - id: entry
        type: u3
        repeat: expr
          - size: this.m_parent.m_parent.logical_screen_descriptor.packed_fields.global_color_table_size
          - size: this.m_parent.packed_fields.local_color_table_size
  lzw_data:
    seq:
      - id: min_code_size
        type: u1
      - id: data_subblocks
        type: data_subblock
        repeat: true
        until: this.data_subblocks[-1].size == 0

  data_subblock:
    seq:
      - id: size
        type: u1
      - id: data
        type: u1
        repeat: size

  extension_data:
    seq:
      - id: subblocks
        type: data_subblock
        repeat: true
        until: this.subblocks[-1].size == 0

