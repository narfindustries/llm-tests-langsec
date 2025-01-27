let DICOMFile = sequence {
    preamble: Preamble;
    prefix: Prefix;
    header: Header;
    data: Data;
};

let Preamble = sequence {
    magic: MagicNumber;
    reserved: Reserved;
};

let MagicNumber = bytes(128);

let Reserved = bytes(128);

let Prefix = bytes(4);

let Header = sequence {
    group: uint16le;
    element: uint16le;
    vr: VR;
    length: Length;
    value: Value;
};

let VR = bytes(2);

let Length = switch (VR) {
    "OB" | "OW" | "SQ" | "UN" => uint32le;
    default => uint16le;
};

let Value = switch (VR) {
    "AE" => ApplicationEntity;
    "AS" => AgeString;
    "AT" => AttributeTag;
    "CS" => CodeString;
    "DA" => Date;
    "DS" => DecimalString;
    "DT" => DateTime;
    "FL" => FloatingPointSingle;
    "FD" => FloatingPointDouble;
    "IS" => IntegerString;
    "LO" => LongString;
    "LT" => LongText;
    "OB" => OtherByte;
    "OD" => OtherDouble;
    "OF" => OtherFloat;
    "OL" => OtherLong;
    "OW" => OtherWord;
    "PN" => PersonName;
    "SH" => ShortString;
    "SL" => SignedLong;
    "SQ" => Sequence;
    "SS" => SignedShort;
    "ST" => ShortText;
    "TM" => Time;
    "UI" => UniqueIdentifier;
    "UL" => UnsignedLong;
    "UN" => Unknown;
    "UR" => URI;
    "US" => UnsignedShort;
    "UT" => UnlimitedText;
};

let ApplicationEntity = bytes(16);

let AgeString = bytes(4);

let AttributeTag = bytes(4);

let CodeString = bytes(16);

let Date = bytes(8);

let DecimalString = bytes(16);

let DateTime = bytes(26);

let FloatingPointSingle = float32le;

let FloatingPointDouble = float64le;

let IntegerString = bytes(12);

let LongString = bytes(64);

let LongText = bytes(10240);

let OtherByte = bytes(Length);

let OtherDouble = bytes(Length);

let OtherFloat = bytes(Length);

let OtherLong = bytes(Length);

let OtherWord = bytes(Length);

let PersonName = bytes(64);

let ShortString = bytes(16);

let SignedLong = int32le;

let Sequence = bytes(Length);

let SignedShort = int16le;

let ShortText = bytes(1024);

let Time = bytes(16);

let UniqueIdentifier = bytes(64);

let UnsignedLong = uint32le;

let Unknown = bytes(Length);

let URI = bytes(Length);

let UnsignedShort = uint16le;

let UnlimitedText = bytes(Length);

let Data = bytes(Length);