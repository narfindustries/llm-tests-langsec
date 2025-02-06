module TIFF;

import std::{uint16, uint32, array};

type WORD = uint16;
type DWORD = uint32;
type LONG = int32;
type RATIONAL = struct {
    numerator : DWORD;
    denominator : DWORD;
};

type IFD_Entry = struct {
    Tag : WORD;
    Type : WORD;
    Count : DWORD;
    Value_Offset : DWORD;
};

type IFD = struct {
    NumEntries : WORD;
    Entries : array<IFD_Entry> of NumEntries;
    NextIFDOffset : DWORD;
};

type TIFF_Header = struct {
    ByteOrder : WORD;
    MagicNumber : WORD;
    IFDOffset : DWORD;
};

type ImageFileDirectory = struct {
    IFDs : array<IFD> while not @.NextIFDOffset == 0;
};

type TIFF_File = struct {
    Header : TIFF_Header;
    FirstIFD : @ImageFileDirectory(Header.IFDOffset);
};

root TIFF_File;