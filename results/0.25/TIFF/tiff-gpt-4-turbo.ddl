module TIFF;

import std.core;

type WORD = UInt16;
type DWORD = UInt32;
type LONG = UInt32;

enum Endian {
  LittleEndian = 0x4949,
  BigEndian = 0x4D4D
}

struct Rational {
  numerator : DWORD;
  denominator : DWORD;
}

struct TIFFHeader {
  endian : Endian;
  magic : WORD = 42;
  ifdOffset : DWORD;
}

enum FieldType {
  BYTE = 1,
  ASCII,
  SHORT,
  LONG,
  RATIONAL,
  SBYTE,
  UNDEFINED,
  SSHORT,
  SLONG,
  SRATIONAL,
  FLOAT,
  DOUBLE
}

struct IFDEntry {
  tag : WORD;
  fieldType : FieldType;
  count : DWORD;
  valueOffset : DWORD;
}

struct IFD {
  numEntries : WORD;
  entries : [IFDEntry] @numEntries;
  nextIFD : DWORD;
}

struct ImageFileDirectory {
  ifds : IFD @^;
}

struct TIFFFile {
  header : TIFFHeader;
  ifd : ImageFileDirectory @header.ifdOffset;
}

type TIFF = TIFFFile;