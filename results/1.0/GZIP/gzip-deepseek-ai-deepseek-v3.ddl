parser GZIP =
  header = block
    id1 = UInt8
    id2 = UInt8
    compression_method = UInt8
    flags = UInt8
    modification_time = UInt32
    extra_flags = UInt8
    operating_system = UInt8
    extra_field = if (flags & 0x04) != 0 then block
      xlen = UInt16
      extra_data = Bytes xlen
    else
      Accept
    original_filename = if (flags & 0x08) != 0 then
      CString
    else
      Accept
    comment = if (flags & 0x10) != 0 then
      CString
    else
      Accept
    header_crc16 = if (flags & 0x02) != 0 then
      UInt16
    else
      Accept
  compressed_data = CompressedBytes
  footer = block
    crc32 = UInt32
    uncompressed_size = UInt32
  in block
    header
    compressed_data
    footer