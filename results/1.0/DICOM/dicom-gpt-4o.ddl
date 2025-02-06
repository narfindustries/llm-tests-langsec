struct DICOM_File {
    preamble: bytes(size = 128);  // 128-byte preamble
    prefix: bytes(size = 4);  // Should be "DICM"
    elements: DataElement[];  // List of data elements
}

struct DataElement {
    tag: uint16le;
    element: uint16le;
    vr: VR = if (isExplicitVR(tag, element)) {
        explicitVR();
    } else {
        implicitVR(tag, element);
    };
    reserved: bytes(size = (isExplicitAndLongVR(vr.vrCode) ? 2 : 0));
    length: uint32le = if (isExplicitAndLongVR(vr.vrCode)) {
        $parent.read("uint32le");
    } else {
        $parent.read("uint16le");
    };
    value: bytes(size = length);
}

struct VR {
    vrCode: string(size = 2);
}

function explicitVR(): VR {
    return VR {
        vrCode: string(2)
    };
}

function isExplicitVR(tag: uint16le, element: uint16le): bool {
    // A hypothetical lookup for explicit VR determination
    return true;
}

function isExplicitAndLongVR(vrCode: string): bool {
    return vrCode in ["OB", "OW", "OF", "SQ", "UT", "UN"];
}

function implicitVR(tag: uint16le, element: uint16le): VR {
    // Example only; real implementation would map tag-element to VR
    return VR { vrCode: "UN" };
}