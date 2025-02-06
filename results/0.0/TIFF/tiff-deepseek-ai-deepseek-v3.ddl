TIFF {
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
        Type: UInt16 where $ >= 1 && $ <= 12;
        Count: UInt32;
        ValueOffset: UInt32;
    }

    Field {
        case Tag {
            254 => NewSubfileType: UInt32;
            255 => SubfileType: UInt16;
            256 => ImageWidth: UInt32;
            257 => ImageLength: UInt32;
            258 => BitsPerSample: UInt16[Count];
            259 => Compression: UInt16 where $ >= 1 && $ <= 32773;
            262 => PhotometricInterpretation: UInt16 where $ >= 0 && $ <= 8;
            263 => Thresholding: UInt16 where $ >= 1 && $ <= 3;
            266 => FillOrder: UInt16 where $ == 1 || $ == 2;
            269 => DocumentName: String;
            270 => ImageDescription: String;
            271 => Make: String;
            272 => Model: String;
            273 => StripOffsets: UInt32[Count];
            274 => Orientation: UInt16 where $ >= 1 && $ <= 8;
            277 => SamplesPerPixel: UInt16;
            278 => RowsPerStrip: UInt32;
            279 => StripByteCounts: UInt32[Count];
            280 => MinSampleValue: UInt16[Count];
            281 => MaxSampleValue: UInt16[Count];
            282 => XResolution: Rational;
            283 => YResolution: Rational;
            284 => PlanarConfiguration: UInt16 where $ == 1 || $ == 2;
            285 => PageName: String;
            286 => XPosition: Rational;
            287 => YPosition: Rational;
            288 => FreeOffsets: UInt32[Count];
            289 => FreeByteCounts: UInt32[Count];
            290 => GrayResponseUnit: UInt16 where $ >= 1 && $ <= 5;
            291 => GrayResponseCurve: UInt16[Count];
            292 => T4Options: UInt32;
            293 => T6Options: UInt32;
            296 => ResolutionUnit: UInt16 where $ >= 1 && $ <= 3;
            297 => PageNumber: UInt16[2];
            305 => Software: String;
            306 => DateTime: String;
            315 => Artist: String;
            316 => HostComputer: String;
            320 => ColorMap: UInt16[Count];
            321 => HalftoneHints: UInt16[2];
            322 => TileWidth: UInt32;
            323 => TileLength: UInt32;
            324 => TileOffsets: UInt32[Count];
            325 => TileByteCounts: UInt32[Count];
            326 => BadFaxLines: UInt32;
            327 => CleanFaxData: UInt16 where $ >= 0 && $ <= 2;
            328 => ConsecutiveBadFaxLines: UInt32;
            330 => SubIFDs: UInt32[Count];
            332 => InkSet: UInt16 where $ == 1 || $ == 2;
            333 => InkNames: String;
            334 => NumberOfInks: UInt16;
            336 => DotRange: UInt16[Count];
            337 => TargetPrinter: String;
            338 => ExtraSamples: UInt16[Count];
            339 => SampleFormat: UInt16[Count];
            340 => SMinSampleValue: UInt32[Count];
            341 => SMaxSampleValue: UInt32[Count];
            342 => TransferRange: UInt16[6];
            343 => ClipPath: UInt8[Count];
            344 => XClipPathUnits: Int32;
            345 => YClipPathUnits: Int32;
            346 => Indexed: UInt16 where $ == 0 || $ == 1;
            347 => JPEGTables: UInt8[Count];
            512 => JPEGProc: UInt16 where $ == 1 || $ == 14;
            513 => JPEGInterchangeFormat: UInt32;
            514 => JPEGInterchangeFormatLength: UInt32;
            515 => JPEGRestartInterval: UInt16;
            517 => JPEGLosslessPredictors: UInt16[Count];
            518 => JPEGPointTransforms: UInt16[Count];
            519 => JPEGQTables: UInt32[Count];
            520 => JPEGDCTables: UInt32[Count];
            521 => JPEGACTables: UInt32[Count];
            529 => YCbCrCoefficients: Rational[3];
            530 => YCbCrSubSampling: UInt16[2];
            531 => YCbCrPositioning: UInt16 where $ == 1 || $ == 2;
            532 => ReferenceBlackWhite: Rational[6];
            700 => XMP: UInt8[Count];
            33432 => Copyright: String;
            34665 => ExifIFD: UInt32;
            34853 => GPSInfo: UInt32;
            37724 => Photoshop: UInt8[Count];
        }
    }

    Rational {
        Numerator: UInt32;
        Denominator: UInt32;
    }

    String {
        Length: UInt32;
        Value: UInt8[Length];
    }
}