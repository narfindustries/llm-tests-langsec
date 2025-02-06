meta:
  id: tiff
  file-extension: tiff
  endian: le
seq:
  - id: header
    type: tiff_header
  - id: ifds
    type: ifd
    repeat: eos
types:
  tiff_header:
    seq:
      - id: byte_order
        type: u2
        enum: byte_order
      - id: version
        type: u2
      - id: first_ifd_offset
        type: u4
  ifd:
    seq:
      - id: num_entries
        type: u2
      - id: entries
        type: ifd_entry
        repeat: expr
        repeat-expr: num_entries
      - id: next_ifd_offset
        type: u4
  ifd_entry:
    seq:
      - id: tag
        type: u2
        enum: tag_type
      - id: field_type
        type: u2
        enum: field_type_enum
      - id: count
        type: u4
      - id: value_offset
        type: u4
  field_type_enum:
    enums:
      type_1: {id: 1, value: byte}
      type_2: {id: 2, value: ascii}
      type_3: {id: 3, value: short}
      type_4: {id: 4, value: long}
      type_5: {id: 5, value: rational}
      type_6: {id: 6, value: sbyte}
      type_7: {id: 7, value: undefined}
      type_8: {id: 8, value: sshort}
      type_9: {id: 9, value: slong}
      type_10: {id: 10, value: srational}
      type_11: {id: 11, value: float}
      type_12: {id: 12, value: double}
  tag_type:
    enums:
      tag_256: {id: 256, value: ImageWidth}
      tag_257: {id: 257, value: ImageLength}
      tag_258: {id: 258, value: BitsPerSample}
      tag_259: {id: 259, value: Compression}
      tag_262: {id: 262, value: PhotometricInterpretation}
      tag_273: {id: 273, value: StripOffsets}
      tag_277: {id: 277, value: SamplesPerPixel}
      tag_278: {id: 278, value: RowsPerStrip}
      tag_279: {id: 279, value: StripByteCounts}
      tag_282: {id: 282, value: XResolution}
      tag_283: {id: 283, value: YResolution}
      tag_296: {id: 296, value: ResolutionUnit}
      tag_274: {id: 274, value: Orientation}
      tag_284: {id: 284, value: PlanarConfiguration}
      tag_320: {id: 320, value: ColorMap}
      tag_338: {id: 338, value: ExtraSamples}
      tag_339: {id: 339, value: SampleFormat}
      tag_315: {id: 315, value: Artist}
      tag_306: {id: 306, value: DateTime}
      tag_269: {id: 269, value: DocumentName}
      tag_305: {id: 305, value: Software}
      tag_33432: {id: 33432, value: Copyright}
      tag_332: {id: 332, value: InkSet}
      tag_336: {id: 336, value: DotRange}
      tag_337: {id: 337, value: TargetPrinter}
      tag_512: {id: 512, value: JPEGProc}
      tag_513: {id: 513, value: JPEGInterchangeFormat}
      tag_514: {id: 514, value: JPEGInterchangeFormatLength}
      tag_321: {id: 321, value: HalftoneHints}
      tag_322: {id: 322, value: TileWidth}
      tag_323: {id: 323, value: TileLength}
      tag_324: {id: 324, value: TileOffsets}
      tag_325: {id: 325, value: TileByteCounts}
  byte_order:
    enums:
      byte_order_4949: {id: 0x4949, value: little_endian}
      byte_order_4d4d: {id: 0x4d4d, value: big_endian}