domain GIF {
  type bytes = sequence of byte;

  type Header = structure {
    field magic: bytes(3) = "GIF";
    field version: bytes(3);
  };

  type LogicalScreenDescriptor = structure {
    field width: uint16;
    field height: uint16;
    field flags: byte;
    field bg_color: byte;
    field aspect_ratio: byte;
  };

  type ColorTableEntry = structure {
    field red: byte;
    field green: byte;
    field blue: byte;
  };

  type ColorTable = sequence of ColorTableEntry;

  type ImageDescriptor = structure {
    field left: uint16;
    field top: uint16;
    field width: uint16;
    field height: uint16;
    field flags: byte;
  };

  type LZWCode = sequence of byte;

  type DataBlock = structure {
    field size: byte;
    field data: bytes(this.size);
  };

  type Data = sequence of DataBlock;

  type Trailer = byte = 0x3B;

  type GIF = sequence {
    item: Header,
    item: LogicalScreenDescriptor,
    optional item: ColorTable,
    zero_or_more item: ImageDescriptor,
    zero_or_more item: Data,
    item: Trailer,
  };
}