domain gif {
  type byte = uint8;
  type word  = uint16;
  type dword = uint32;

  type gif_header {
    signature: byte[3] = [0x47, 0x49, 0x46];
    version: byte[3];
  }

  type logical_screen_descriptor {
    width: word;
    height: word;
    flags: byte;
    bg_color: byte;
    pixel_aspect_ratio: byte;
  }

  type color_table {
    size: dword;
    colors: byte[size * 3];
  }

  type image_descriptor {
    left: word;
    top: word;
    width: word;
    height: word;
    flags: byte;
  }

  type pixel_data {
    min_code_size: byte;
    blocks: byte[];
  }

  type gif_image {
    header: gif_header;
    screen_descriptor: logical_screen_descriptor;
    global_color_table: color_table;
    images: image_descriptor[];
    pixel_data: pixel_data[];
  }

  grammar gif_file {
    gif_image;
  }
}