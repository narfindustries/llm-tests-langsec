domain gif {
  type Header = struct {
    sig: byte[3],
    version: byte[3]
  }

  type LogicalScreenDescriptor = struct {
    width: uint16,
    height: uint16,
    flags: byte,
    bg_color: byte,
    pixel_aspect_ratio: byte
  }

  type ColorTable = array[256] of byte[3]

  type ImageDescriptor = struct {
    left: uint16,
    top: uint16,
    width: uint16,
    height: uint16,
    flags: byte
  }

  type PixelData = array của uint8

  type Trailer = byte

  root_type ImageFile

  type ImageFile = struct {
    header: Header,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: optional ColorTable,
    data: array of (ImageDescriptor and PixelData and optional ColorTable),
    trailer: Trailer
  }
}