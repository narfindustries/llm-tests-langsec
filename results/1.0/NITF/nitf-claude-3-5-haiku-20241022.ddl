Here's the complete Daedalus specification, revised to address potential compilation issues:

%NITF 2.1 Specification
%Claude AI Generated Specification
%Date: 2024-10-22

type Nitf {
    header: NitfHeader,
    imageSegments: [ImageSegment],
    graphicSegments: [GraphicSegment],
    textSegments: [TextSegment],
    dataExtSegments: [DataExtSegment]
}

type NitfHeader {
    fileProfileName: string(9),
    fileVersion: string(4),
    securityClassification: string(1),
    securityCategoryCode: string(3),
    originStationId: string(10),
    fileDateTime: string(14),
    fileTitle: string(80),
    securityGroup: SecurityGroup,
    standardsIndicator: string(2),
    originatorName: string(27),
    phoneNumber: string(18)
}

type SecurityGroup {
    securityClassification: string(1),
    securityControlAndReleasingMarking: string(2),
    securityDowngradeDate: string(8),
    securityDowngradeEvent: string(40)
}

type ImageSegment {
    imageSubheader: ImageSubheader,
    imageData: bytes
}

type ImageSubheader {
    imageLength: uint32,
    imageWidth: uint32,
    imageMode: string(1),
    compressionType: string(2)
}

type GraphicSegment {
    graphicSubheader: GraphicSubheader,
    graphicData: bytes
}

type GraphicSubheader {
    graphicStyle: string(2),
    graphicColor: string(1)
}

type TextSegment {
    textSubheader: TextSubheader,
    textData: string
}

type TextSubheader {
    textTitle: string(80),
    textFormat: string(3)
}

type DataExtSegment {
    dataExtSubheader: DataExtSubheader,
    dataExtData: bytes
}

type DataExtSubheader {
    dataExtType: string(2),
    dataExtLength: uint32
}

parser NitfParser {
    Nitf {
        header: parseNitfHeader(),
        imageSegments: parseImageSegments(),
        graphicSegments: parseGraphicSegments(),
        textSegments: parseTextSegments(),
        dataExtSegments: parseDataExtSegments()
    }
}

func parseNitfHeader() -> NitfHeader {
    NitfHeader {
        fileProfileName: read_string(9),
        fileVersion: read_string(4),
        securityClassification: read_string(1),
        securityCategoryCode: read_string(3),
        originStationId: read_string(10),
        fileDateTime: read_string(14),
        fileTitle: read_string(80),
        securityGroup: parseSecurityGroup(),
        standardsIndicator: read_string(2),
        originatorName: read_string(27),
        phoneNumber: read_string(18)
    }
}

func parseSecurityGroup() -> SecurityGroup {
    SecurityGroup {
        securityClassification: read_string(1),
        securityControlAndReleasingMarking: read_string(2),
        securityDowngradeDate: read_string(8),
        securityDowngradeEvent: read_string(40)
    }
}

func parseImageSegments() -> [ImageSegment] {
    repeat {
        ImageSegment {
            imageSubheader: parseImageSubheader(),
            imageData: read_bytes(imageSubheader.imageLength)
        }
    }
}

func parseImageSubheader() -> ImageSubheader {
    ImageSubheader {
        imageLength: read_uint32(),
        imageWidth: read_uint32(),
        imageMode: read_string(1),
        compressionType: read_string(2)
    }
}

func parseGraphicSegments() -> [GraphicSegment] {
    repeat {
        GraphicSegment {
            graphicSubheader: parseGraphicSubheader(),
            graphicData: read_bytes(graphicSubheader.graphicStyle.length)
        }
    }
}

func parseGraphicSubheader() -> GraphicSubheader {
    GraphicSubheader {
        graphicStyle: read_string(2),
        graphicColor: read_string(1)
    }
}

func parseTextSegments() -> [TextSegment] {
    repeat {
        TextSegment {
            textSubheader: parseTextSubheader(),
            textData: read_string(textSubheader.textTitle.length)
        }
    }
}

func parseTextSubheader() -> TextSubheader {
    TextSubheader {
        textTitle: read_string(80),
        textFormat: read_string(3)
    }
}

func parseDataExtSegments() -> [DataExtSegment] {
    repeat {
        DataExtSegment {
            dataExtSubheader: parseDataExtSubheader(),
            dataExtData: read_bytes(dataExtSubheader.dataExtLength)
        }
    }
}

func parseDataExtSubheader() -> DataExtSubheader {
    DataExtSubheader {
        dataExtType: read_string(2),
        dataExtLength: read_uint32()
    }
}