let DICOMFile = sequence {
    preamble: Preamble;
    prefix: Prefix;
    header: Header;
    data: Data;
};

let Preamble = sequence {
    magic: Magic;
    reserved: Reserved;
};

let Magic = bytes(128);

let Reserved = bytes(12);

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
    "AE" | "AS" | "AT" | "CS" | "DA" | "DS" | "DT" | "FL" | "FD" | "IS" | "LO" | "LT" | "PN" | "SH" | "SL" | "SS" | "ST" | "TM" | "UI" | "UL" | "US" | "UT" => bytes(Length);
    "OB" | "OW" | "SQ" | "UN" => bytes(Length);
};

let Data = bytes(..);