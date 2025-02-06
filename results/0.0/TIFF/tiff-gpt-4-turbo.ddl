grammar Tiff;

import std::int;
import std::array;
import std::vector;

type Rational = struct {
    numerator : uint32;
    denominator : uint32;
};

type IFDEntry = struct {
    tag : uint16;
    fieldType : uint16;
    count : uint32;
    valueOffset : uint32;
};

type IFD = struct {
    numEntries : uint16;
    entries : vector<IFDEntry>(numEntries);
    nextIFDOffset : uint32;
};

type TIFFHeader = struct {
    endian : array[uint8, 2];
    magicNumber : uint16;
    ifdOffset : uint32;
};

type TIFFFile = struct {
    header : TIFFHeader;
    ifds : vector<IFD>;
};

type ImageData = struct {
    strips : vector<array[uint8]>;
};

type TIFF = struct {
    file : TIFFFile;
    imageData : ImageData;
};

// Helper functions to determine endianess
func readUInt16(data : array[uint8, 2], bigEndian : bool) -> uint16 {
    if bigEndian {
        return (uint16(data[0]) << 8) | uint16(data[1]);
    } else {
        return (uint16(data[1]) << 8) | uint16(data[0]);
    }
}

func readUInt32(data : array[uint8, 4], bigEndian : bool) -> uint32 {
    if bigEndian {
        return (uint32(data[0]) << 24) | (uint32(data[1]) << 16) | (uint32(data[2]) << 8) | uint32(data[3]);
    } else {
        return (uint32(data[3]) << 24) | (uint32(data[2]) << 16) | (uint32(data[1]) << 8) | uint32(data[0]);
    }
}

// Main parsing logic
func parseTIFF(data : array[uint8]) -> TIFF {
    let bigEndian = (data[0] == 0x4D && data[1] == 0x4D);
    let header = TIFFHeader {
        endian : data[0..2],
        magicNumber : readUInt16(data[2..4], bigEndian),
        ifdOffset : readUInt32(data[4..8], bigEndian)
    };

    var ifds : vector<IFD>;
    var offset = header.ifdOffset;
    while (offset != 0) {
        let numEntries = readUInt16(data[offset..offset+2], bigEndian);
        var entries : vector<IFDEntry>;
        offset += 2;
        for i in 0..numEntries {
            entries.push(IFDEntry {
                tag : readUInt16(data[offset..offset+2], bigEndian),
                fieldType : readUInt16(data[offset+2..offset+4], bigEndian),
                count : readUInt32(data[offset+4..offset+8], bigEndian),
                valueOffset : readUInt32(data[offset+8..offset+12], bigEndian)
            });
            offset += 12;
        }
        let nextIFDOffset = readUInt32(data[offset..offset+4], bigEndian);
        ifds.push(IFD {
            numEntries : numEntries,
            entries : entries,
            nextIFDOffset : nextIFDOffset
        });
        offset = nextIFDOffset;
    }

    // Assuming imageData parsing is handled elsewhere based on IFD entries
    let imageData = ImageData {
        strips : vector<array[uint8]> // Placeholder
    };

    return TIFF {
        file : TIFFFile {
            header : header,
            ifds : ifds
        },
        imageData : imageData
    };
}