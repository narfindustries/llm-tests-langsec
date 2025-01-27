namespace DICOM {
    type DICOMFile = struct {
        preamble: Preamble;
        prefix: Prefix;
        elements: list<Element>;
    };

    type Preamble = struct {
        data: bytes(128);
    };

    type Prefix = struct {
        data: bytes(4);
    };

    type Element = struct {
        tag: Tag;
        vr: VR;
        length: Length;
        value: Value;
    };

    type Tag = struct {
        group: uint16;
        element: uint16;
    };

    type VR = enum : string {
        "AE", "AS", "AT", "CS", "DA", "DS", "DT", "FL", "FD", "IS", "LO", "LT", "OB", "OD", "OF", "OL", "OW", "PN", "SH", "SL", "SQ", "SS", "ST", "TM", "UC", "UI", "UL", "UN", "UR", "US", "UT"
    };

    type Length = uint32;

    type Value = union {
        case vr == "AE" : string;
        case vr == "AS" : string;
        case vr == "AT" : list<uint16>;
        case vr == "CS" : string;
        case vr == "DA" : string;
        case vr == "DS" : string;
        case vr == "DT" : string;
        case vr == "FL" : float32;
        case vr == "FD" : float64;
        case vr == "IS" : string;
        case vr == "LO" : string;
        case vr == "LT" : string;
        case vr == "OB" : bytes(length);
        case vr == "OD" : bytes(length);
        case vr == "OF" : bytes(length);
        case vr == "OL" : bytes(length);
        case vr == "OW" : bytes(length);
        case vr == "PN" : string;
        case vr == "SH" : string;
        case vr == "SL" : int32;
        case vr == "SQ" : Sequence;
        case vr == "SS" : int16;
        case vr == "ST" : string;
        case vr == "TM" : string;
        case vr == "UC" : string;
        case vr == "UI" : string;
        case vr == "UL" : uint32;
        case vr == "UN" : bytes(length);
        case vr == "UR" : string;
        case vr == "US" : uint16;
        case vr == "UT" : string;
    };

    type Sequence = list<Element>;
}