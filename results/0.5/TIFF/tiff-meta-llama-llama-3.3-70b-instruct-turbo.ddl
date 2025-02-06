format tiff: 
  magic: uint16,
  ifd_offset: uint32

format ifd: 
  entry_count: uint16,
  entries: array[entry_count] of ifd_entry,
  next_ifd_offset: uint32

format ifd_entry: 
  tag: uint16,
  type: uint16,
  count: uint32,
  value: bytes

format tag_256_image_width: 
  tag: uint16 = 256,
  type: uint16 = 3 or 4,
  count: uint32 = 1,
  value: uint16 or uint32

format tag_257_image_length: 
  tag: uint16 = 257,
  type: uint16 = 3 or 4,
  count: uint32 = 1,
  value: uint16 or uint32

format tag_258_bits_per_sample: 
  tag: uint16 = 258,
  type: uint16 = 3,
  count: uint32,
  value: array[count] of uint16

format tag_259_compression: 
  tag: uint16 = 259,
  type: uint16 = 3,
  count: uint32 = 1,
  value: uint16 = 1 or 2 or 3 or 4 or 5

format tag_262_photometric_interpretation: 
  tag: uint16 = 262,
  type: uint16 = 3,
  count: uint32 = 1,
  value: uint16 = 0 or 1 or 2 or 3 or 4 or 5

format tag_274_orientation: 
  tag: uint16 = 274,
  type: uint16 = 3,
  count: uint32 = 1,
  value: uint16 = 1 or 2 or 3 or 4 or 5 or 6 or 7 or 8

format tag_277_samples_per_pixel: 
  tag: uint16 = 277,
  type: uint16 = 3,
  count: uint32 = 1,
  value: uint16 = 1 or 2 or 3 or 4

format tag_278_rows_per_strip: 
  tag: uint16 = 278,
  type: uint16 = 3 or 4,
  count: uint32 = 1,
  value: uint16 or uint32

format tag_279_strip_offsets: 
  tag: uint16 = 279,
  type: uint16 = 3 or 4,
  count: uint32,
  value: array[count] of uint16 or uint32

format tag_280_strip_byte_counts: 
  tag: uint16 = 280,
  type: uint16 = 3 or 4,
  count: uint32,
  value: array[count] of uint16 or uint32

format tag_282_x_resolution: 
  tag: uint16 = 282,
  type: uint16 = 5,
  count: uint32 = 1,
  value: rational

format tag_283_y_resolution: 
  tag: uint16 = 283,
  type: uint16 = 5,
  count: uint32 = 1,
  value: rational

format tag_284_planar_configuration: 
  tag: uint16 = 284,
  type: uint16 = 3,
  count: uint32 = 1,
  value: uint16 = 1 or 2

format tag_285_page_name: 
  tag: uint16 = 285,
  type: uint16 = 2,
  count: uint32,
  value: bytes

format tag_286_x_position: 
  tag: uint16 = 286,
  type: uint16 = 3 or 4,
  count: uint32 = 1,
  value: uint16 or uint32

format tag_287_y_position: 
  tag: uint16 = 287,
  type: uint16 = 3 or 4,
  count: uint32 = 1,
  value: uint16 or uint32

format tag_288_free_offsets: 
  tag: uint16 = 288,
  type: uint16 = 3 or 4,
  count: uint32 = 1,
  value: uint16 or uint32

format tag_289_free_byte_counts: 
  tag: uint16 = 289,
  type: uint16 = 3 or 4,
  count: uint32 = 1,
  value: uint16 or uint32

format tag_290_gray_response_unit: 
  tag: uint16 = 290,
  type: uint16 = 3,
  count: uint32 = 1,
  value: uint16 = 1 or 2

format tag_291_gray_response_curve: 
  tag: uint16 = 291,
  type: uint16 = 3,
  count: uint32,
  value: array[count] of uint16

format tag_292_t4_options: 
  tag: uint16 = 292,
  type: uint16 = 3,
  count: uint32 = 1,
  value: uint16 = 0 or 1

format tag_293_t6_options: 
  tag: uint16 = 293,
  type: uint16 = 3,
  count: uint32 = 1,
  value: uint16 = 0 or 1

format tag_296_resolution_unit: 
  tag: uint16 = 296,
  type: uint16 = 3,
  count: uint32 = 1,
  value: uint16 = 1 or 2 or 3

format tag_297_page_number: 
  tag: uint16 = 297,
  type: uint16 = 3,
  count: uint32 = 2,
  value: array[count] of uint16

format tag_301_transfer_function: 
  tag: uint16 = 301,
  type: uint16 = 3,
  count: uint32,
  value: array[count] of uint16

format tag_305_software: 
  tag: uint16 = 305,
  type: uint16 = 2,
  count: uint32,
  value: bytes

format tag_306_date_time: 
  tag: uint16 = 306,
  type: uint16 = 2,
  count: uint32 = 20,
  value: bytes

format tag_315_artist: 
  tag: uint16 = 315,
  type: uint16 = 2,
  count: uint32,
  value: bytes

format tag_316_host_computer: 
  tag: uint16 = 316,
  type: uint16 = 2,
  count: uint32,
  value: bytes

format tag_317_predictor: 
  tag: uint16 = 317,
  type: uint16 = 3,
  count: uint32 = 1,
  value: uint16 = 1 or 2

format tag_318_white_point: 
  tag: uint16 = 318,
  type: uint16 = 5,
  count: uint32 = 2,
  value: array[count] of rational

format tag_319_primary_chromaticities: 
  tag: uint16 = 319,
  type: uint16 = 5,
  count: uint32 = 6,
  value: array[count] of rational

format tag_320_color_map: 
  tag: uint16 = 320,
  type: uint16 = 3,
  count: uint32,
  value: array[count] of uint16

format tag_321_halftone_hints: 
  tag: uint16 = 321,
  type: uint16 = 3,
  count: uint32 = 2,
  value: array[count] of uint16

format tag_322_tile_width: 
  tag: uint16 = 322,
  type: uint16 = 3 or 4,
  count: uint32 = 1,
  value: uint16 or uint32

format tag_323_tile_length: 
  tag: uint16 = 323,
  type: uint16 = 3 or 4,
  count: uint32 = 1,
  value: uint16 or uint32

format tag_324_tile_offsets: 
  tag: uint16 = 324,
  type: uint16 = 3 or 4,
  count: uint32,
  value: array[count] of uint16 or uint32

format tag_325_tile_byte_counts: 
  tag: uint16 = 325,
  type: uint16 = 3 or 4,
  count: uint32,
  value: array[count] of uint16 or uint32

format tag_332_ink_set: 
  tag: uint16 = 332,
  type: uint16 = 3,
  count: uint32 = 1,
  value: uint16 = 1 or 2

format tag_333_ink_names: 
  tag: uint16 = 333,
  type: uint16 = 2,
  count: uint32,
  value: bytes

format tag_334_number_of_inks: 
  tag: uint16 = 334,
  type: uint16 = 3,
  count: uint32 = 1,
  value: uint16

format tag_336_dot_range: 
  tag: uint16 = 336,
  type: uint16 = 3,
  count: uint32 = 2,
  value: array[count] of uint16

format tag_337_target_printer: 
  tag: uint16 = 337,
  type: uint16 = 2,
  count: uint32,
  value: bytes

format tag_338_extra_samples: 
  tag: uint16 = 338,
  type: uint16 = 3,
  count: uint32 = 1,
  value: uint16 = 0 or 1 or 2

format tag_339_sample_format: 
  tag: uint16 = 339,
  type: uint16 = 3,
  count: uint32,
  value: array[count] of uint16 = 1 or 2 or 3

format tag_340_s_min_sample_value: 
  tag: uint16 = 340,
  type: uint16 = 3 or 5 or 9 or 11,
  count: uint32,
  value: array[count] of uint16 or rational or float

format tag_341_s_max_sample_value: 
  tag: uint16 = 341,
  type: uint16 = 3 or 5 or 9 or 11,
  count: uint32,
  value: array[count] of uint16 or rational or float

format tag_342_transfer_range: 
  tag: uint16 = 342,
  type: uint16 = 3,
  count: uint32 = 2,
  value: array[count] of uint16

format tag_343_clip_path: 
  tag: uint16 = 343,
  type: uint16 = 3,
  count: uint32,
  value: bytes

format tag_344_x_clip_path_units: 
  tag: uint16 = 344,
  type: uint16 = 3,
  count: uint32 = 1,
  value: uint16 = 1 or 2

format tag_345_y_clip_path_units: 
  tag: uint16 = 345,
  type: uint16 = 3,
  count: uint32 = 1,
  value: uint16 = 1 or 2

format tag_346_indexed: 
  tag: uint16 = 346,
  type: uint16 = 3,
  count: uint32 = 1,
  value: uint16 = 0 or 1

format tag_347_jpeg_tables: 
  tag: uint16 = 347,
  type: uint16 = 7,
  count: uint32,
  value: bytes

format tag_351_opi_proxy: 
  tag: uint16 = 351,
  type: uint16 = 2,
  count: uint32,
  value: bytes

format tiff_file: 
  magic: uint16 = 0x4949 or 0x4D4D,
  ifd_offset: uint32,
  ifd: ifd,
  image_data: bytes