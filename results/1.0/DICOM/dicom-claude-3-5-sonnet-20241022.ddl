specification DicomParser {
    import RegularExpressions;

    token PairedElements = bitstring;
    token DicomTag = bitstring;
    token VRCode = bitstring;
    token ValueLength = bitstring;
    token ValueField = bitstring;
    token ObjectID = bitstring;

    struct DicomValue {
        @ordered
        tag: DicomTag;
        vr: VRCode;
        length: ValueLength;
        value: ValueField;
    }

    struct DicomSequence {
        @ordered
        sequenceTag: DicomTag;
        vr: VRCode;
        length: ValueLength;
        items: Array DicomItem;
    }

    struct DicomItem {
        @ordered
        values: Array DicomValue;
        sequences: Array DicomSequence;
    }

    struct DicomDataset {
        @ordered
        preamble: PairedElements;
        prefix: ObjectID;
        items: Array DicomItem;
    }

    parser DataSetParser: DicomDataset = {
        let preamble = $PairedElements;
        let prefix = $ObjectID;
        let items = many DicomItemParser;
        return DicomDataset {
            preamble = preamble,
            prefix = prefix,
            items = items
        };
    }

    parser DicomItemParser: DicomItem = {
        let values = many DicomValueParser;
        let sequences = many DicomSequenceParser;
        return DicomItem {
            values = values,
            sequences = sequences
        };
    }

    parser DicomValueParser: DicomValue = {
        let tag = $DicomTag;
        let vr = $VRCode;
        let length = $ValueLength;
        let value = $ValueField;
        return DicomValue {
            tag = tag,
            vr = vr,
            length = length,
            value = value
        };
    }

    parser DicomSequenceParser: DicomSequence = {
        let sequenceTag = $DicomTag;
        let vr = $VRCode;
        let length = $ValueLength;
        let items = many DicomItemParser;
        return DicomSequence {
            sequenceTag = sequenceTag,
            vr = vr,
            length = length,
            items = items
        };
    }

    main = DataSetParser;
}