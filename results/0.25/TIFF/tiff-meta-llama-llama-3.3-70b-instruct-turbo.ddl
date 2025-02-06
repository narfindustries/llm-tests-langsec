format tiff {
  byte_order: uint16,
  tiff_identifier: uint16 = 0x002a,
  offset_to_first_ifd: uint32,
}

format ifd {
  num_directory_entries: uint16,
  directory_entries: array of directory_entry,
}

format directory_entry {
  tag: uint16,
  type: uint16,
  count: uint32,
  value_offset: uint32,
}

enum tag {
  image_width = 0x0100,
  image_length = 0x0101,
  bits_per_sample = 0x0102,
  compression = 0x0103,
  photometric_interpretation = 0x0106,
  threshholding = 0x0107,
  cell_length = 0x0108,
  cell_width = 0x0109,
  fill_order = 0x010a,
  document_name = 0x010d,
  image_description = 0x010e,
  make = 0x010f,
  model = 0x0110,
  strip_offsets = 0x0111,
  orientation = 0x0112,
  samples_per_pixel = 0x0113,
  rows_per_strip = 0x0114,
  strip_byte_counts = 0x0115,
  min_sample_value = 0x0116,
  max_sample_value = 0x0117,
  x_resolution = 0x0118,
  y_resolution = 0x0119,
  planar_configuration = 0x011a,
  page_name = 0x011b,
  x_position = 0x011c,
  y_position = 0x011d,
  free_offsets = 0x011e,
  free_byte_counts = 0x011f,
  gray_response_unit = 0x0120,
  gray_response_curve = 0x0121,
  color_response_unit = 0x0122,
  color_response_curve = 0x0123,
  t4_options = 0x0124,
  t6_options = 0x0125,
  resolution_unit = 0x0128,
  page_number = 0x0129,
  software = 0x0131,
  date_time = 0x0132,
  artist = 0x013b,
  host_computer = 0x013c,
  color_map = 0x0140,
  halftone_hints = 0x0141,
  tile_width = 0x0142,
  tile_length = 0x0143,
  tile_offsets = 0x0144,
  tile_byte_counts = 0x0145,
  bad_fax_lines = 0x0146,
  clean_fax_data = 0x0147,
  consecutive_bad_fax_lines = 0x0148,
  sub_ifd = 0x0149,
  ink_set = 0x014a,
  ink_names = 0x014c,
  number_of_inks = 0x014d,
  dot_range = 0x0150,
  target_printer = 0x0151,
  extra_samples = 0x0152,
  sample_format = 0x0153,
  s_min_sample_value = 0x0154,
  s_max_sample_value = 0x0155,
  transfer_range = 0x0156,
  jpeg_proc = 0x0157,
  jpeg_interchange_format = 0x0158,
  jpeg_interchange_format_length = 0x0159,
  jpeg_restart_interval = 0x015a,
  jpeg_lossless_predictors = 0x015b,
  jpeg_point_transforms = 0x015c,
  jpeg_q_tables = 0x015d,
  jpeg_dctables = 0x015e,
  jpeg_actables = 0x015f,
}

enum type {
  byte = 1,
  ascii = 2,
  short = 3,
  long = 4,
  rational = 5,
  undefined = 7,
  slong = 9,
  srational = 10,
}

enum compression {
  none = 1,
  ccitt_group_3 = 2,
  ccitt_group_4 = 3,
  lzw = 4,
  packbits = 5,
  jpeg = 6,
}

enum photometric_interpretation {
  white_is_zero = 0,
  black_is_zero = 1,
  rgb = 2,
  palette_color = 3,
  transparency_mask = 4,
  cmyk = 5,
  ycbcr = 6,
}

enum fill_order {
  msb_to_lsb = 1,
  lsb_to_msb = 2,
}

enum orientation {
  top_left = 1,
  top_right = 2,
  bottom_right = 3,
  bottom_left = 4,
  left_top = 5,
  right_top = 6,
  right_bottom = 7,
  left_bottom = 8,
}

enum planar_configuration {
  chunky = 1,
  planar = 2,
}

enum resolution_unit {
  none = 1,
  inch = 2,
  centimeter = 3,
}

enum sample_format {
  unsigned_integer = 1,
  signed_integer = 2,
  floating_point = 3,
  undefined = 4,
}