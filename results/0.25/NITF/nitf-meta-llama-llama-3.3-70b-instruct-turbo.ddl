type NITF = struct {
  header_segment: Header;
  image_segment: [Image];
  graphic_segment: [Graphic];
  text_segment: [Text];
  data_extension_segment: [DataExtension];
  trailer_segment: Trailer;
};

type Header = struct {
  file_header: bytes : size(4) = "NITF";
  file_version_number: uint16;
  complex_header_type: string : size(25);
  header_length: uint24;
  num_image_segments: uint8;
  num_graphic_segments: uint8;
  num_text_segments: uint8;
  num_data_extension_segments: uint8;
  header_extension: [HeaderExtension];
  file_time: string : size(6);
  file_date: string : size(6);
  file_title: string : size(20);
  security_classification: enum { U, C, S, T };
  classification_authority: string : size(20);
  classification_reason: string : size(20);
  downgrade: uint8;
  downgrade_date: string : size(6);
  classification_text: string : size(43);
  encrypted_header: bool;
  encryption_algorithm: enum { DES: 1, TripleDES: 2, AES: 3 };
};

type HeaderExtension = struct {
  extension_type: string : size(25);
  extension_data: bytes;
};

type Image = struct {
  image_identifier: string : size(25);
  image_date_time: string : size(12);
  image_title: string : size(20);
  image_security_classification: enum { U, C, S, T };
  image_compression: enum { None: 0, JPEG: 1, CCITTGroup4: 2 };
  image_type: enum { Monochrome: 1, Color: 2 };
  image_representation: enum { PixelInterleaved: 1, LineInterleaved: 2 };
  image_pixel_type: enum { Uint8: 1, Int16: 2 };
  image_pixel_size: enum { Byte: 1, Word: 2 };
  image_dimensions: uint32;
  image_data: bytes;
};

type Graphic = struct {
  graphic_identifier: string : size(25);
  graphic_date_time: string : size(12);
  graphic_title: string : size(20);
  graphic_security_classification: enum { U, C, S, T };
  graphic_type: enum { Vector: 1, Raster: 2 };
  graphic_representation: enum { PixelInterleaved: 1, LineInterleaved: 2 };
  graphic_data: bytes;
};

type Text = struct {
  text_identifier: string : size(25);
  text_date_time: string : size(12);
  text_title: string : size(20);
  text_security_classification: enum { U, C, S, T };
  text_type: enum { ASCII: 1, Unicode: 2 };
  text_data: bytes;
};

type DataExtension = struct {
  data_extension_identifier: string : size(25);
  data_extension_date_time: string : size(12);
  data_extension_title: string : size(20);
  data_extension_security_classification: enum { U, C, S, T };
  data_extension_type: enum { XML: 1, JSON: 2 };
  data_extension_data: bytes;
};

type Trailer = struct {
  trailer: bytes : size(4) = "NIET";
};