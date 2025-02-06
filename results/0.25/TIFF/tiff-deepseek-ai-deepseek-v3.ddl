tiff {
    Header {
        ByteOrder: UInt16 where $ == 0x4949 || $ == 0x4D4D;
        MagicNumber: UInt16 where $ == 42;
        IFDOffset: UInt32;
    }

    IFD {
        NumEntries: UInt16;
        Entries: Entry[NumEntries];
        NextIFDOffset: UInt32;
    }

    Entry {
        Tag: UInt16;
        FieldType: UInt16;
        Count: UInt32;
        ValueOffset: UInt32;
    }

    FieldType {
        Byte: UInt8 where $ == 1;
        ASCII: UInt8 where $ == 2;
        Short: UInt16 where $ == 3;
        Long: UInt32 where $ == 4;
        Rational: UInt64 where $ == 5;
        SByte: Int8 where $ == 6;
        Undefined: UInt8 where $ == 7;
        SShort: Int16 where $ == 8;
        SLong: Int32 where $ == 9;
        SRational: Int64 where $ == 10;
        Float: Float32 where $ == 11;
        Double: Float64 where $ == 12;
    }

    ImageWidth: Entry where Tag == 256;
    ImageLength: Entry where Tag == 257;
    BitsPerSample: Entry where Tag == 258;
    Compression: Entry where Tag == 259;
    PhotometricInterpretation: Entry where Tag == 262;
    StripOffsets: Entry where Tag == 273;
    SamplesPerPixel: Entry where Tag == 277;
    RowsPerStrip: Entry where Tag == 278;
    StripByteCounts: Entry where Tag == 279;
    XResolution: Entry where Tag == 282;
    YResolution: Entry where Tag == 283;
    ResolutionUnit: Entry where Tag == 296;
    PlanarConfiguration: Entry where Tag == 284;
    GrayResponseUnit: Entry where Tag == 290;
    ColorMap: Entry where Tag == 320;
    ExtraSamples: Entry where Tag == 338;
    Orientation: Entry where Tag == 274;
    FillOrder: Entry where Tag == 266;
    DocumentName: Entry where Tag == 269;
    PageName: Entry where Tag == 285;
    Software: Entry where Tag == 305;
    DateTime: Entry where Tag == 306;
    Artist: Entry where Tag == 315;
    HostComputer: Entry where Tag == 316;
    Predictor: Entry where Tag == 317;
    WhitePoint: Entry where Tag == 318;
    PrimaryChromaticities: Entry where Tag == 319;
    TileWidth: Entry where Tag == 322;
    TileLength: Entry where Tag == 323;
    TileOffsets: Entry where Tag == 324;
    TileByteCounts: Entry where Tag == 325;
    SubIFDs: Entry where Tag == 330;
    JPEGTables: Entry where Tag == 347;
    YCbCrCoefficients: Entry where Tag == 529;
    YCbCrSubSampling: Entry where Tag == 530;
    YCbCrPositioning: Entry where Tag == 531;
    ReferenceBlackWhite: Entry where Tag == 532;
    Copyright: Entry where Tag == 33432;
    ModelPixelScaleTag: Entry where Tag == 33550;
    ModelTiepointTag: Entry where Tag == 33922;
    GeoKeyDirectoryTag: Entry where Tag == 34735;
    GeoDoubleParamsTag: Entry where Tag == 34736;
    GeoAsciiParamsTag: Entry where Tag == 34737;
}