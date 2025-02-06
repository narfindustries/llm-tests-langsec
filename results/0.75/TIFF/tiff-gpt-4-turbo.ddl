module Tiff;

import std::number::{u8, u16, u32, be, le};
import std::collections::seq;

type ImageFileDirectoryEntry = struct {
    Tag: u16,
    Type: u16,
    Count: u32,
    ValueOffset: u32
};

type ImageFileDirectory = struct {
    NumEntries: u16,
    Entries: seq::sized(ImageFileDirectoryEntry, NumEntries),
    NextIFDOffset: u32
};

type Rational = struct {
    Numerator: u32,
    Denominator: u32
};

type TIFFHeader = struct {
    ByteOrder: u16,
    Version: u16,
    FirstIFDOffset: u32
};

type TIFFFile = struct {
    Header: TIFFHeader,
    IFDs: seq::offset(ImageFileDirectory, Header.FirstIFDOffset),
    SubIFDs: seq::offset(ImageFileDirectory, ImageFileDirectory.NextIFDOffset)
};

derive endian from Header -> (
    if Header.ByteOrder == 0x4949 then le else be
);

root TIFFFile;