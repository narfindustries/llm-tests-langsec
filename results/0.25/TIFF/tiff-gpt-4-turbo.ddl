module TIFF {

  type ByteOrder = enum {
    LittleEndian = 0x4949,
    BigEndian = 0x4D4D
  }

  type IFD_Entry = struct {
    tag   : uint16,
    type  : uint16,
    count : uint32,
    value : uint32
  }

  type IFD = struct {
    numEntries : uint16,
    entries    : array[IFD_Entry] (numEntries),
    nextIFD    : uint32
  }

  type TIFF_Header = struct {
    byteOrder : ByteOrder,
    magic     : uint16,
    offset    : uint32
  }

  type TIFF_File = struct {
    header : TIFF_Header,
    ifds   : array[IFD] (computeNumIFDs(header.offset, current_offset()))
  }

  let computeNumIFDs(startOffset: uint32, currentOffset: uint32) : uint32 = {
    var offset = startOffset
    var count = 0u32
    while (offset != 0u32 && offset < currentOffset) {
      let ifd = parse IFD at offset
      offset = ifd.nextIFD
      count += 1
    }
    count
  }

  let main = parse TIFF_File
}