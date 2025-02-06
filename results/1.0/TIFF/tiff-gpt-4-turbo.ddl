module Tiff;

type TiffHeader = struct {
    endianTag: (uint8, uint8) @assert ($1 == 'II' || $1 == 'MM'); // Little endian or Big endian
    version: uint16 @bigendian;
    firstIFDOffset: uint32 @bigendian;
};

type IFDEntry = struct {
    tag: uint16 @bigendian;
    type: uint16 @bigendian;
    count: uint32 @bigendian;
    valueOffset: uint32 @bigendian;
};

type IFD = struct {
    numEntries: uint16 @bigendian;
    entries: [numEntries] of IFDEntry;
    nextIFDOffset: uint32 @bigendian;
};

type TIFFFile = struct {
    header: TiffHeader;
    ifds: [IFD] @cursor(header.firstIFDOffset) @until($1.nextIFDOffset == 0);
};