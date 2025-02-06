def TIFF {
    type ByteOrder = 
      | LittleEndian = 0x4949
      | BigEndian = 0x4D4D

    type MagicNumber = u16(0x2A)

    type Tag =
      | NewSubfileType = 0x00FE
      | ImageWidth = 0x0100
      | ImageLength = 0x0101
      | BitsPerSample = 0x0102
      | Compression = 0x0103
      | PhotometricInterpretation = 0x0106
      | FillOrder = 0x010A
      | DocumentName = 0x010D
      | ImageDescription = 0x010E
      | Make = 0x010F
      | Model = 0x0110
      | StripOffsets = 0x0111
      | Orientation = 0x0112
      | SamplesPerPixel = 0x0115
      | RowsPerStrip = 0x0116
      | StripByteCounts = 0x0117
      | XResolution = 0x011A
      | YResolution = 0x011B
      | PlanarConfiguration = 0x011C
      | ResolutionUnit = 0x0128
      | Software = 0x0131
      | DateTime = 0x0132
      | Artist = 0x013B
      | Predictor = 0x013D
      | ColorMap = 0x0140
      | SampleFormat = 0x0153

    type FieldType =
      | Byte = 1
      | ASCII = 2
      | Short = 3
      | Long = 4
      | Rational = 5
      | SByte = 6
      | Undefined = 7
      | SShort = 8
      | SLong = 9
      | SRational = 10
      | Float = 11
      | Double = 12

    type Compression =
      | NoCompression = 1
      | ModifiedHuffmanRLE = 2
      | Group3Fax = 3
      | Group4Fax = 4
      | LZW = 5
      | JPEG = 6
      | PackBits = 32773

    type PhotometricInterpretation =
      | WhiteIsZero = 0
      | BlackIsZero = 1
      | RGB = 2
      | PaletteColor = 3
      | TransparencyMask = 4
      | CMYK = 5
      | YCbCr = 6
      | CIELab = 8

    type Orientation =
      | TopLeft = 1
      | TopRight = 2
      | BottomRight = 3
      | BottomLeft = 4
      | LeftTop = 5
      | RightTop = 6
      | RightBottom = 7
      | LeftBottom = 8

    type ResolutionUnit =
      | NoUnit = 1
      | Inch = 2
      | Centimeter = 3

    type PlanarConfiguration =
      | Chunky = 1
      | Planar = 2

    type SampleFormat =
      | UnsignedInteger = 1
      | SignedInteger = 2
      | FloatingPoint = 3
      | Undefined = 4

    struct IFDEntry {
        tag: Tag;
        field_type: FieldType;
        value_count: u32;
        value_or_offset: u32
    }

    struct TIFFHeader {
        byte_order: ByteOrder;
        magic_number: MagicNumber;
        ifd_offset: u32
    }

    struct ImageFileDirectory {
        num_entries: u16;
        entries: IFDEntry[];
        next_ifd_offset: u32
    }

    struct TIFF {
        header: TIFFHeader;
        image_file_directories: ImageFileDirectory[]
    }

    parse TIFF {
        header = TIFFHeader {
            byte_order: match input[0:2] {
                0x4949 => ByteOrder.LittleEndian,
                0x4D4D => ByteOrder.BigEndian,
                _ => error("Invalid byte order")
            },
            magic_number: input[2:4] as MagicNumber,
            ifd_offset: input[4:8] as u32
        };

        current_offset = header.ifd_offset;
        image_file_directories = [];

        while current_offset != 0 {
            num_entries = parse_u16(input[current_offset:current_offset+2]);
            entries = [];

            for i in 0..num_entries {
                entry_offset = current_offset + 2 + (i * 12);
                entries.append(IFDEntry {
                    tag: parse_u16(input[entry_offset:entry_offset+2]) as Tag,
                    field_type: parse_u16(input[entry_offset+2:entry_offset+4]) as FieldType,
                    value_count: parse_u32(input[entry_offset+4:entry_offset+8]),
                    value_or_offset: parse_u32(input[entry_offset+8:entry_offset+12])
                });
            }

            next_ifd_offset = parse_u32(input[current_offset+2+(num_entries*12):current_offset+2+(num_entries*12)+4]);

            image_file_directories.append(ImageFileDirectory {
                num_entries: num_entries,
                entries: entries,
                next_ifd_offset: next_ifd_offset
            });

            current_offset = next_ifd_offset;
        }

        return TIFF {
            header: header,
            image_file_directories: image_file_directories
        };
    }
}