#! ddl 1.0

// TIFF Binary Format Specification
tiff = {
    header : TiffHeader
    ifd    : IFD 
}

TiffHeader = {
    byteOrder = ( | 'II' | 'MM' )
    version   = 42
    ifdOffset : U32be
}

IFD = {
    entryCount : U16be
    entries    : Array entryCount IFDEntry
    nextIFD    : U32be
}

IFDEntry = {
    tag         : U16be
    dataType    : U16be
    dataCount   : U32be
    dataOffset  : U32be
}

// Data Types
U16be = IntBe 2
U32be = IntBe 4

// Main Parser
Main = tiff