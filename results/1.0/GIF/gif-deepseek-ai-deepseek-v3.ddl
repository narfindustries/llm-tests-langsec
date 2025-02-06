let Header = {
  signature: @b"GIF",
  version: match @b"87a" | @b"89a"
};

let PackedFields = {
  global_color_table_flag: bool,
  color_resolution: uint(3),
  sort_flag: bool,
  size_of_gct: uint(3)
};

let LogicalScreenDescriptor = {
  screen_width: uint(16),
  screen_height: uint(16),
  packed_fields: PackedFields,
  background_color_index: uint(8),
  pixel_aspect_ratio: uint(8)
};

let ColorTable(size: int) = array(size) of {
  red: uint(8),
  green: uint(8),
  blue: uint(8)
};

let ImageDescriptor = {
  image_separator: @b"\x2C",
  image_left_position: uint(16),
  image_top_position: uint(16),
  image_width: uint(16),
  image_height: uint(16),
  packed_fields: {
    local_color_table_flag: bool,
    interlace_flag: bool,
    sort_flag: bool,
    reserved: uint(2),
    size_of_lct: uint(3)
  }
};

let ImageData = {
  lzw_minimum_code_size: uint(8),
  compressed_data_blocks: repeat {
    block_size: uint(8),
    data: bytes(block_size)
  } until block_size == 0
};

let GraphicControlExtension = {
  extension_introducer: @b"\x21",
  graphic_control_label: @b"\xF9",
  block_size: @b"\x04",
  packed_fields: {
    reserved: uint(3),
    disposal_method: uint(3),
    user_input_flag: bool,
    transparent_color_flag: bool
  },
  delay_time: uint(16),
  transparent_color_index: uint(8),
  block_terminator: @b"\x00"
};

let PlainTextExtension = {
  extension_introducer: @b"\x21",
  plain_text_label: @b"\x01",
  block_size: @b"\x0C",
  text_grid_left: uint(16),
  text_grid_top: uint(16),
  text_grid_width: uint(16),
  text_grid_height: uint(16),
  character_cell_width: uint(8),
  character_cell_height: uint(8),
  text_foreground_color_index: uint(8),
  text_background_color_index: uint(8),
  text_data: repeat {
    block_size: uint(8),
    data: bytes(block_size)
  } until block_size == 0,
  block_terminator: @b"\x00"
};

let CommentExtension = {
  extension_introducer: @b"\x21",
  comment_label: @b"\xFE",
  comment_data: repeat {
    block_size: uint(8),
    data: bytes(block_size)
  } until block_size == 0,
  block_terminator: @b"\x00"
};

let ApplicationExtension = {
  extension_introducer: @b"\x21",
  application_label: @b"\xFF",
  block_size: @b"\x0B",
  application_identifier: bytes(8),
  application_authentication_code: bytes(3),
  application_data: repeat {
    block_size: uint(8),
    data: bytes(block_size)
  } until block_size == 0,
  block_terminator: @b"\x00"
};

let Image = {
  descriptor: ImageDescriptor,
  local_color_table: if descriptor.packed_fields.local_color_table_flag
    then ColorTable(size: 1 << (descriptor.packed_fields.size_of_lct + 1))
    else nil,
  image_data: ImageData
};

let Block = {
  block: match {
    image: Image,
    extension: match {
      graphic_control: GraphicControlExtension,
      plain_text: PlainTextExtension,
      comment: CommentExtension,
      application: ApplicationExtension
    }
  }
};

let GIF = {
  header: Header,
  logical_screen_descriptor: LogicalScreenDescriptor,
  global_color_table: if logical_screen_descriptor.packed_fields.global_color_table_flag
    then ColorTable(size: 1 << (logical_screen_descriptor.packed_fields.size_of_gct + 1))
    else nil,
  blocks: repeat {
    block: Block
  },
  trailer: @b"\x3B"
};