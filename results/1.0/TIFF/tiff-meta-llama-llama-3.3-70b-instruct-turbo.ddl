format Daedalus

type TIFF = 
  struct {
    byteorder: uint8 = 0x49, # 0x49 for little-endian, 0x4d for big-endian
    tiff_magic: uint16 = 0x002a,
    ifd_offset: uint32,
    ifds: array [IFD]
  }

type IFD = 
  struct {
    num_entries: uint16,
    entries: array [IFD_Entry] of num_entries,
    next_ifd_offset: uint32
  }

type IFD_Entry = 
  struct {
    tag: uint16,
    type: uint16,
    count: uint32,
    value: Value
  }

type Value = 
  union {
    uint8_value: uint8,
    uint16_value: uint16,
    uint32_value: uint32,
    string_value: string,
    rational_value: Rational,
    byte_array_value: byte array
  }

type Rational = 
  struct {
    numerator: uint32,
    denominator: uint32
  }

type ImageWidthEntry = 
  struct {
    tag: uint16 = 256,
    type: uint16 = 3,
    count: uint32 = 1,
    value: uint16_value: uint16
  }

type ImageLengthEntry = 
  struct {
    tag: uint16 = 257,
    type: uint16 = 3,
    count: uint32 = 1,
    value: uint16_value: uint16
  }

type BitsPerSampleEntry = 
  struct {
    tag: uint16 = 258,
    type: uint16 = 3,
    count: uint32,
    value: byte_array_value: byte array
  }

type CompressionEntry = 
  struct {
    tag: uint16 = 259,
    type: uint16 = 3,
    count: uint32 = 1,
    value: uint16_value: uint16
  }

type PhotometricInterpretationEntry = 
  struct {
    tag: uint16 = 262,
    type: uint16 = 3,
    count: uint32 = 1,
    value: uint16_value: uint16
  }

type OrientationEntry = 
  struct {
    tag: uint16 = 274,
    type: uint16 = 3,
    count: uint32 = 1,
    value: uint16_value: uint16
  }

type SamplesPerPixelEntry = 
  struct {
    tag: uint16 = 277,
    type: uint16 = 3,
    count: uint32 = 1,
    value: uint8_value: uint8
  }

type RowsPerStripEntry = 
  struct {
    tag: uint16 = 278,
    type: uint16 = 3 | 4,
    count: uint32 = 1,
    value: uint16_value | uint32_value: uint16 | uint32
  }

type StripOffsetsEntry = 
  struct {
    tag: uint16 = 279,
    type: uint16 = 3 | 4,
    count: uint32,
    value: byte_array_value: byte array
  }

type StripByteCountsEntry = 
  struct {
    tag: uint16 = 280,
    type: uint16 = 3 | 4,
    count: uint32,
    value: byte_array_value: byte array
  }

type MinSampleValueEntry = 
  struct {
    tag: uint16 = 281,
    type: uint16 = 3,
    count: uint32 = 1,
    value: uint16_value: uint16
  }

type MaxSampleValueEntry = 
  struct {
    tag: uint16 = 282,
    type: uint16 = 3,
    count: uint32 = 1,
    value: uint16_value: uint16
  }

type XResolutionEntry = 
  struct {
    tag: uint16 = 282,
    type: uint16 = 5,
    count: uint32 = 1,
    value: rational_value: Rational
  }

type YResolutionEntry = 
  struct {
    tag: uint16 = 283,
    type: uint16 = 5,
    count: uint32 = 1,
    value: rational_value: Rational
  }

type PlanarConfigurationEntry = 
  struct {
    tag: uint16 = 284,
    type: uint16 = 3,
    count: uint32 = 1,
    value: uint16_value: uint16
  }

type PageNameEntry = 
  struct {
    tag: uint16 = 285,
    type: uint16 = 2,
    count: uint32,
    value: string_value: string
  }

type XPositionEntry = 
  struct {
    tag: uint16 = 286,
    type: uint16 = 5,
    count: uint32 = 1,
    value: rational_value: Rational
  }

type YPositionEntry = 
  struct {
    tag: uint16 = 287,
    type: uint16 = 5,
    count: uint32 = 1,
    value: rational_value: Rational
  }

type FreeOffsetsEntry = 
  struct {
    tag: uint16 = 288,
    type: uint16 = 3 | 4,
    count: uint32,
    value: byte_array_value: byte array
  }

type FreeByteCountsEntry = 
  struct {
    tag: uint16 = 289,
    type: uint16 = 3 | 4,
    count: uint32,
    value: byte_array_value: byte array
  }

type GrayResponseUnitEntry = 
  struct {
    tag: uint16 = 290,
    type: uint16 = 3,
    count: uint32 = 1,
    value: uint16_value: uint16
  }

type GrayResponseCurveEntry = 
  struct {
    tag: uint16 = 291,
    type: uint16 = 3,
    count: uint32,
    value: byte_array_value: byte array
  }

type T4OptionsEntry = 
  struct {
    tag: uint16 = 292,
    type: uint16 = 3,
    count: uint32 = 1,
    value: uint16_value: uint16
  }

type T6OptionsEntry = 
  struct {
    tag: uint16 = 293,
    type: uint16 = 3,
    count: uint32 = 1,
    value: uint16_value: uint16
  }

type ResolutionUnitEntry = 
  struct {
    tag: uint16 = 296,
    type: uint16 = 3,
    count: uint32 = 1,
    value: uint16_value: uint16
  }

type PageNumberEntry = 
  struct {
    tag: uint16 = 297,
    type: uint16 = 3,
    count: uint32 = 2,
    value: byte_array_value: byte array
  }

type ColorResponseUnitEntry = 
  struct {
    tag: uint16 = 300,
    type: uint16 = 3,
    count: uint32 = 1,
    value: uint16_value: uint16
  }

type ColorResponseCurveEntry = 
  struct {
    tag: uint16 = 301,
    type: uint16 = 3,
    count: uint32,
    value: byte_array_value: byte array
  }

type TransferFunctionEntry = 
  struct {
    tag: uint16 = 301,
    type: uint16 = 3,
    count: uint32,
    value: byte_array_value: byte array
  }

type SoftwareEntry = 
  struct {
    tag: uint16 = 305,
    type: uint16 = 2,
    count: uint32,
    value: string_value: string
  }

type DateTimeEntry = 
  struct {
    tag: uint16 = 306,
    type: uint16 = 2,
    count: uint32,
    value: string_value: string
  }

type ArtistEntry = 
  struct {
    tag: uint16 = 315,
    type: uint16 = 2,
    count: uint32,
    value: string_value: string
  }

type HostComputerEntry = 
  struct {
    tag: uint16 = 316,
    type: uint16 = 2,
    count: uint32,
    value: string_value: string
  }

type PredictorEntry = 
  struct {
    tag: uint16 = 317,
    type: uint16 = 3,
    count: uint32 = 1,
    value: uint16_value: uint16
  }

type WhitePointEntry = 
  struct {
    tag: uint16 = 318,
    type: uint16 = 5,
    count: uint32 = 1,
    value: rational_value: Rational
  }

type PrimaryChromaticitiesEntry = 
  struct {
    tag: uint16 = 319,
    type: uint16 = 5,
    count: uint32 = 1,
    value: rational_value: Rational
  }

type ColorMapEntry = 
  struct {
    tag: uint16 = 320,
    type: uint16 = 3,
    count: uint32,
    value: byte_array_value: byte array
  }

type HalftoneHintsEntry = 
  struct {
    tag: uint16 = 321,
    type: uint16 = 3,
    count: uint32 = 2,
    value: byte_array_value: byte array
  }

type TileWidthEntry = 
  struct {
    tag: uint16 = 322,
    type: uint16 = 3 | 4,
    count: uint32 = 1,
    value: uint16_value | uint32_value: uint16 | uint32
  }

type TileLengthEntry = 
  struct {
    tag: uint16 = 323,
    type: uint16 = 3 | 4,
    count: uint32 = 1,
    value: uint16_value | uint32_value: uint16 | uint32
  }

type TileOffsetsEntry = 
  struct {
    tag: uint16 = 324,
    type: uint16 = 3 | 4,
    count: uint32,
    value: byte_array_value: byte array
  }

type TileByteCountsEntry = 
  struct {
    tag: uint16 = 325,
    type: uint16 = 3 | 4,
    count: uint32,
    value: byte_array_value: byte array
  }

type SubIFDOffsetEntry = 
  struct {
    tag: uint16 = 330,
    type: uint16 = 3 | 4,
    count: uint32 = 1,
    value: uint16_value | uint32_value: uint16 | uint32
  }

type InkSetEntry = 
  struct {
    tag: uint16 = 332,
    type: uint16 = 3,
    count: uint32 = 1,
    value: uint16_value: uint16
  }

type InkNamesEntry = 
  struct {
    tag: uint16 = 333,
    type: uint16 = 2,
    count: uint32,
    value: string_value: string
  }

type NumberOfInksEntry = 
  struct {
    tag: uint16 = 334,
    type: uint16 = 3,
    count: uint32 = 1,
    value: uint16_value: uint16
  }

type DotRangeEntry = 
  struct {
    tag: uint16 = 336,
    type: uint16 = 3,
    count: uint32 = 2,
    value: byte_array_value: byte array
  }

type TargetPrinterEntry = 
  struct {
    tag: uint16 = 337,
    type: uint16 = 2,
    count: uint32,
    value: string_value: string
  }

type ExtraSamplesEntry = 
  struct {
    tag: uint16 = 338,
    type: uint16 = 3,
    count: uint32,
    value: byte_array_value: byte array
  }

type SampleFormatEntry = 
  struct {
    tag: uint16 = 339,
    type: uint16 = 3,
    count: uint32 = 1,
    value: uint16_value: uint16
  }

type SMinSampleValueEntry = 
  struct {
    tag: uint16 = 340,
    type: uint16 = 3,
    count: uint32 = 1,
    value: uint16_value: uint16
  }

type SMaxSampleValueEntry = 
  struct {
    tag: uint16 = 341,
    type: uint16 = 3,
    count: uint32 = 1,
    value: uint16_value: uint16
  }