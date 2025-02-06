module NITF {
    type FileHeader = struct {
        header: Ascii("NITF02.10"),
        complexityLevel: U8,
        securityType: Enum("R", "C", "S", "T"),
        originatingStationId: String(10),
        fileLength: U32,
        encryption: Enum("0", "1"),
        securityClassification: struct {
            securityLevel: Enum("TOP_SECRET", "SECRET", "CONFIDENTIAL", "RESTRICTED"),
            securityControlMarkings: Optional(String),
            codewords: Optional(String),
            controlAndHandling: Optional(String),
            releaseMarking: Optional(String)
        },
        fileDateTime: String(14),
        titleField: Optional(String(80)),
        securityGroupExtension: Optional(struct {
            extendedHeaderDataLength: U16,
            extendedHeaderData: Bytes
        })
    }

    type ImageSegment = struct {
        imageSubheader: struct {
            imageIdentifier: String(10),
            imageDateTime: String(14),
            targetIdentifier: Optional(String),
            imageSource: String(42),
            significantRows: U32,
            significantColumns: U32,
            pixelValueType: Enum("INT", "B", "SI", "R", "C"),
            imageRepresentation: Enum("MONO", "RGB", "MULTI"),
            imageCategory: Enum("VIS", "SL", "IR", "SAR", "XRAY", "THERMAL", "ACOUSTIC"),
            actualBitsPerPixel: U8,
            compression: Enum("NC", "NM", "C1", "C3", "C4", "C5", "C6", "C7", "C8"),
            compressionRate: Optional(String),
            imageMode: Enum("B", "P", "R", "S"),
            numberBands: U8,
            bandInfo: List(struct {
                bandRepresentation: Enum("VIS", "IR", "SAR", "MULTI"),
                bandSubcategory: Optional(String),
                bandWeights: Optional(F32)
            }),
            imageSecurityClassification: struct {
                securityLevel: Enum("TOP_SECRET", "SECRET", "CONFIDENTIAL", "RESTRICTED"),
                securityControlMarkings: Optional(String),
                codewords: Optional(String)
            },
            imageExtendedSubheader: Optional(struct {
                extendedHeaderDataLength: U16,
                extendedHeaderData: Bytes
            })
        },
        imageData: Bytes
    }

    type GraphicSegment = Optional(List(struct {
        graphicSubheader: struct {
            graphicIdentifier: String(10),
            graphicName: Optional(String),
            graphicType: Enum("CGM", "BITMAP", "VECTOR"),
            graphicColor: Optional(String),
            graphicSecurityClassification: struct {
                securityLevel: Enum("TOP_SECRET", "SECRET", "CONFIDENTIAL", "RESTRICTED")
            }
        },
        graphicData: Bytes
    }))

    type TextSegment = Optional(List(struct {
        textSubheader: struct {
            textIdentifier: String(10),
            textTitle: Optional(String),
            textFormat: Enum("BASICCHAR", "UNICODE"),
            textSecurityClassification: struct {
                securityLevel: Enum("TOP_SECRET", "SECRET", "CONFIDENTIAL", "RESTRICTED")
            }
        },
        textData: String
    }))

    type DataExtensionSegment = Optional(List(struct {
        dataExtensionSubheader: struct {
            dataExtensionType: String,
            dataExtensionTypeDescription: Optional(String),
            dataExtensionSecurityClassification: struct {
                securityLevel: Enum("TOP_SECRET", "SECRET", "CONFIDENTIAL", "RESTRICTED")
            }
        },
        dataExtensionData: Bytes
    }))

    type NITFFile = struct {
        fileHeader: FileHeader,
        imageSegments: List(ImageSegment),
        graphicSegments: GraphicSegment,
        textSegments: TextSegment,
        dataExtensionSegments: DataExtensionSegment
    }
}