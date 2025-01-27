domain gif {
  type byte = uint8;
  type uint16 = uint16;
  type uint32 = uint32;

  type gif_header = struct {
    signature: byte[3];
    version: byte[3];
  };

  type logical_screen_descriptor = struct {
    width: uint16;
    height: uint16;
    flags: byte;
    bg_color: byte;
    pixel_aspect_ratio: byte;
  };

  type image_descriptor = struct {
    left: uint16;
    top: uint16;
    width: uint16;
    height: uint16;
    flags: byte;
  };

  type pixel_data = byte[];

  type gif_image = struct {
    descriptor: image_descriptor;
    data: pixel_data;
  };

  type gif_file = struct {
    header: gif_header;
    screen_descriptor: logical_screen_descriptor;
    images: gif_image[];
  };

  grammar gif_file_grammar {
    gif_file: 
      header: gif_header,
      screen_descriptor: logical_screen_descriptor,
      images: gif_image*;
    gif_header: 
      signature: "GIF",
      version: byte[3];
    logical_screen_descriptor: 
      width: uint16,
      height: uint16,
      flags: byte,
      bg_color: byte,
      pixel_aspect_ratio: byte;
    image_descriptor: 
      left: uint16,
      top: uint16,
      width: uint16,
      height: uint16,
      flags: byte;
    pixel_data: 
      byte*;
    gif_image: 
      descriptor: image_descriptor,
      data: pixel_data;
  }
}