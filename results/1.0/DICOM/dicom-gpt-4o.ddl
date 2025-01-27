module DICOM;

type DicomFile = struct {
    preamble : bytes(128);
    prefix : string(4);
    elements : list<DicomElement>;
};

type DicomElement = struct {
    group : uint16;
    element : uint16;
    vr : string(2);
    length : uint32;
    value : bytes(length);
};

function parse_op(stream : bit_stream, context : parse_context) : parse_result<DicomFile> {
    let preamble = stream.read_bytes(128);
    if not validate_preamble(preamble) {
        return ParseError("Invalid DICOM preamble");
    }

    let prefix = stream.read_string(4);
    if prefix != "DICM" {
        return ParseError("DICOM prefix missing");
    }

    let elements = list<DicomElement>[];
    while not stream.eof() {
        let group = stream.read_uint16();
        let element = stream.read_uint16();
        let vr = stream.read_string(2);
        let length = stream.read_uint32();
        let value = stream.read_bytes(length);

        elements.append(DicomElement{ group, element, vr, length, value });
    }

    return ParseSuccess(DicomFile{ preamble, prefix, elements });
}

function validate_preamble(preamble : bytes) : bool {
    // Add specific checks if necessary
    return true;
}
