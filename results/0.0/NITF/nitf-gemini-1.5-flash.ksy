meta:
  id: nitf-gemini-1
  title: NITF Gemini 1.5 Flash Kaitai Struct
  homepage: https://kaitai.io
  file-extension: .ntf
  experimental: true

types:
  header:
    seq:
      - id: signature
        type: str
        size: 4
      - id: version
        type: u2
      - id: header_length
        type: u4
      - id: file_name
        type: str
        size: 256
      - id: file_size
        type: u8
      - id: image_width
        type: u4
      - id: image_height
        type: u4
      - id: image_data_offset
        type: u8
      - id: image_data_size
        type: u8
      - id: compression_type
        type: u2
      - id: color_space
        type: u2
      - id: bits_per_pixel
        type: u2
      - id: reserved
        type: u4
        size: 16

  image_data:
    seq:
      - id: data
        type: bytes
        size: parent.header.image_data_size


seq:
  - id: header
    type: header
  - id: image_data
    type: image_data

