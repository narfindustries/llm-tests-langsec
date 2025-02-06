data TiffHeader = TiffHeader {
  byteOrder : ByteOrder,
  ifdOffset : Word32
}

data ByteOrder = BigEndian | LittleEndian

data ImageFileDirectory = ImageFileDirectory {
  entries : [TiffEntry]
}

data TiffEntry = TiffEntry {
  tag : Word16,
  type : TiffType,
  count : Word32,
  valueOffset : Word32
}

data TiffType = BYTE | ASCII | SHORT | LONG | RATIONAL | SBYTE | SSHORT | SLONG | SRATIONAL | FLOAT | DOUBLE | IFD | UNDEFINED

data TiffValue =
  TiffByteValue { bytes : [Byte] } |
  TiffAsciiValue { ascii : String } |
  TiffShortValue { shorts : [Word16] } |
  TiffLongValue { longs : [Word32] } |
  TiffRationalValue { rationals : [(Word32, Word32)] } |
  TiffSByteValue { sbytes : [Int8] } |
  TiffSShortValue { sshorts : [Int16] } |
  TiffSLongValue { slongs : [Int32] } |
  TiffSRationalValue { srationals : [(Int32, Int32)] } |
  TiffFloatValue { floats : [Float32] } |
  TiffDoubleValue { doubles : [Float64] } |
  TiffIFDValue { ifdOffset : Word32 } |
  TiffUndefinedValue { undefined : [Byte] }


-- Partial representation.  Many more tags are possible.  This is a highly simplified example. Needs to be extended significantly. Refer to TIFF 6.0 spec.
data TiffTag =
  ImageWidth |
  ImageLength |
  BitsPerSample |
  Compression |
  PhotometricInterpretation |
  StripOffsets |
  StripByteCounts |
  SamplesPerPixel |
  RowsPerStrip |
  PlanarConfiguration |
  ResolutionUnit |
  XResolution |
  YResolution |
  Make |
  Model |
  Software |
  DateTime |
  Artist |
  Copyright


data TiffImage = TiffImage {
  header : TiffHeader,
  ifds : [ImageFileDirectory], --TIFF can have multiple IFDs
  imageData : [Byte] -- Placeholder for image data. Actual structure is complex and depends on many factors.
}
